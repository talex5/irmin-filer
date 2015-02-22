(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Sexplib.Std
open Lwt

open Ck_utils

type uuid = string with sexp

let root_id : uuid = ""

let mint_uuid () = Uuidm.(create `V4 |> to_string)

module Disk_types = struct
  type action = [`Action of Ck_sigs.action_details]
  type project = [`Project of Ck_sigs.project_details]
  type area = [`Area]

  type 'a node = {
    parent : uuid;
    name : string;
    description : string;
    ctime : float with default(0.0);
    details : 'a;
  } with sexp

  type general_node =
    [ `Action of Ck_sigs.action_details
    | `Project of Ck_sigs.project_details
    | `Area ] node
    with sexp
end

let root_node = { Disk_types.
  parent = root_id;
  name = "All";
  description = "Root area";
  details = `Area;
  ctime = 0.0;
}

module Raw(I : Irmin.BASIC with type key = string list and type value = string) = struct
  open Disk_types

  type 'a n = {
    uuid : uuid;
    disk_node : 'a Disk_types.node;
    child_nodes : [area | project | action] n list;
  }

  type t = {
    store : string -> I.t;
    root : 'a. ([> area] as 'a) n;
    index : (uuid, [area | project | action] n) Hashtbl.t;
  }

  let rec walk fn node =
    fn node;
    node.child_nodes |> List.iter (walk fn)

  let by_name a b =
    match String.compare a.disk_node.name b.disk_node.name with
    | 0 -> compare a.uuid b.uuid
    | r -> r

  let make store =
    let disk_nodes = Hashtbl.create 100 in
    let children = Hashtbl.create 100 in
    Hashtbl.add disk_nodes root_id root_node;
    I.list (store "Find db nodes") ["db"] >>=
    Lwt_list.iter_s (function
      | ["db"; uuid] as key ->
          assert (uuid <> root_id);
          I.read_exn (store "Load db node") key >|= fun s ->
          let node = general_node_of_sexp (Sexplib.Sexp.of_string s) in
          Hashtbl.add disk_nodes uuid node;
          let old_children =
            try Hashtbl.find children node.parent
            with Not_found -> [] in
          Hashtbl.replace children node.parent (uuid :: old_children);
      | _ -> assert false
    ) >|= fun () ->
    children |> Hashtbl.iter (fun parent children ->
      if not (Hashtbl.mem disk_nodes parent) then (
        error "Parent UUID '%s' of child nodes %s missing!" parent (String.concat ", " children)
      )
    );

    (* todo: reject cycles *)
    let rec make_node uuid =
      let disk_node = Hashtbl.find disk_nodes uuid in {
        uuid;
        disk_node;
        child_nodes = make_child_nodes uuid;
      }
    and make_child_nodes uuid =
      begin try Hashtbl.find children uuid with Not_found -> [] end
      |> List.map make_node
      |> List.sort by_name in

    let root = {
      uuid = root_id;
      disk_node = root_node;
      child_nodes = make_child_nodes root_id;
    } in
    let index = Hashtbl.create 100 in
    root |> walk (fun node -> Hashtbl.add index node.uuid node);
    { store; root; index }

  let get t uuid =
    try Hashtbl.find t.index uuid
    with Not_found -> error "UUID '%s' not found in database!" uuid

  (* Note: in theory, the result might not match the input type, if the
   * merge changes it for some reason. In practice, this shouldn't happen. *)
  let create t (node:[< action | project | area] node) =
    let node = (node :> general_node) in
    let uuid = mint_uuid () in
    assert (not (Hashtbl.mem t.index uuid));
    if not (Hashtbl.mem t.index node.parent) then
      error "Parent '%s' does not exist!" node.parent;
    let s = Sexplib.Sexp.to_string (sexp_of_general_node node) in
    I.update (t.store "create") ["db"; uuid] s >>= fun () ->
    make t.store >|= fun t_new ->
    (Hashtbl.find t_new.index uuid, t_new)

  let update t (node:[< action | project | area] n) =
    let node = (node :> [action | project | area] n) in
    assert (Hashtbl.mem t.index node.uuid);
    if not (Hashtbl.mem t.index node.disk_node.parent) then
      error "Parent '%s' does not exist!" node.disk_node.parent;
    let s = Sexplib.Sexp.to_string (sexp_of_general_node node.disk_node) in
    I.update (t.store "update") ["db"; node.uuid] s >>= fun () ->
    make t.store
end

module Make(I : Irmin.BASIC with type key = string list and type value = string) = struct
  module R = Raw(I)

  type t = {
    current : R.t React.S.t;
    set_current : R.t -> unit;
  }

  type 'a full_node = 'a R.n

  type area = Disk_types.area
  type project = Disk_types.project
  type action = Disk_types.action

  type node_view = {
    uuid : uuid;
    node_type : [ area | project | action ] React.S.t;
    ctime : float;
    name : string React.S.t;
    child_views : node_view ReactiveData.RList.t;
  }

  type details = {
    details_uuid : uuid;
    details_type : [ area | project | action ] React.S.t;
    details_name : string React.S.t;
    details_description : string React.S.t;
    details_children : node_view ReactiveData.RList.t;
  }

  let make store =
    R.make store >|= fun r ->
    let current, set_current = React.S.create r in
    { current; set_current }

  let root t = t.current |> React.S.map (fun r -> r.R.root)

  let all_areas_and_projects t =
    let results = ref [] in
    let rec scan prefix x =
      let full_path = prefix ^ "/" ^ x.R.disk_node.Disk_types.name in
      results := (full_path, x) :: !results;
      x.R.child_nodes |> List.iter (fun child ->
        match child with
        | {R.disk_node = {Disk_types.details = `Area | `Project _; _}; _} as x -> scan full_path x
        | _ -> ()
      ) in
    scan "" (root t |> React.S.value);
    List.rev !results

  let actions parent =
    let results = ref [] in
    parent.R.child_nodes |> List.iter (fun child ->
      match child with
      | {R.disk_node = {Disk_types.details = `Action _; _}; _} as x -> results := x :: !results
      | _ -> ()
    );
    List.rev !results

  let projects parent =
    let results = ref [] in
    parent.R.child_nodes |> List.iter (fun child ->
      match child with
      | {R.disk_node = {Disk_types.details = `Project _; _}; _} as x -> results := x :: !results
      | _ -> ()
    );
    List.rev !results

  let areas parent =
    let results = ref [] in
    parent.R.child_nodes |> List.iter (fun child ->
      match child with
      | {R.disk_node = {Disk_types.details = `Area; _}; _} as x -> results := x :: !results
      | _ -> ()
    );
    List.rev !results

  let name node = node.R.disk_node.Disk_types.name

  let uuid node = node.R.uuid

  let add details t ~parent ~name ~description =
    let disk_node = { Disk_types.
      name;
      description;
      parent;
      ctime = Unix.gettimeofday ();
      details;
    } in
    let r = React.S.value t.current in
    R.create r disk_node >|= fun (_node, r_new) ->
    t.set_current r_new

  let add_action = add (`Action {Ck_sigs.astate = `Next})
  let add_project = add (`Project {Ck_sigs.pstate = `Active})
  let add_area = add `Area

  let set_name t node name =
    let r = React.S.value t.current in
    let new_node = {node with
      R.disk_node = {node.R.disk_node with Disk_types.name}
    } in
    R.update r new_node >|= t.set_current

  let set_state t uuid new_state =
    let r = React.S.value t.current in
    let node = R.get r uuid in
    let new_node = {node with
      R.disk_node = {node.R.disk_node with Disk_types.details = new_state}
    } in
    R.update r new_node >|= t.set_current

  let node_type {R.disk_node = {Disk_types.details; _}; _} = details

  let process_tree t =
    let rec view node =
      let live_node = t.current |> React.S.map (fun r -> R.get r node.R.uuid) in
      let child_nodes = live_node |> React.S.map (fun node -> node.R.child_nodes) in
      let child_views =
        rlist_of ~init:node.R.child_nodes child_nodes
        |> ReactiveData.RList.map view in
      {
        uuid = node.R.uuid;
        ctime = node.R.disk_node.Disk_types.ctime;
        node_type = live_node |> React.S.map node_type;
        name = live_node |> React.S.map (fun node -> node.R.disk_node.Disk_types.name);
        child_views;
      } in
    let root_node = R.get (React.S.value t.current) root_id in
    view root_node

  let collect_actions r =
    let results = ref [] in
    let rec scan = function
      | {R.disk_node = {Disk_types.details = `Area | `Project _; _}; _} as x ->
          results := actions x @ !results;
          x.R.child_nodes |> List.iter scan
      | {R.disk_node = {Disk_types.details = `Action _; _}; _} -> ()
    in
    scan r.R.root;
    !results

  let render_node node = {
    uuid = node.R.uuid;
    ctime = node.R.disk_node.Disk_types.ctime;
    node_type = React.S.const (node_type node);
    name = React.S.const node.R.disk_node.Disk_types.name;    (* Signal? *)
    child_views = ReactiveData.RList.empty;
  }

  let work_tree t =
    let init = collect_actions (React.S.value t.current) in
    t.current
    |> React.S.map collect_actions
    |> rlist_of ~init
    |> ReactiveData.RList.map render_node

  let leaf_view t uuid =
    let r = React.S.value t.current in
    let node = R.get r uuid in
    render_node node

  let details t uuid =
    let initial_node = R.get (React.S.value t.current) uuid in
    let node = t.current |> React.S.map (fun r -> R.get r uuid) in
    let details_children =
      let child_nodes = node |> React.S.map (fun node -> node.R.child_nodes) in
      rlist_of ~init:initial_node.R.child_nodes child_nodes
      |> ReactiveData.RList.map (fun node -> leaf_view t (node.R.uuid)) in
    {
      details_uuid = initial_node.R.uuid;
      details_type = node |> React.S.map node_type;
      details_name = node |> React.S.map (fun n -> n.R.disk_node.Disk_types.name);
      details_description = node |> React.S.map (fun n -> n.R.disk_node.Disk_types.description);
      details_children;
    }
end
