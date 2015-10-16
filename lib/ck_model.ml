(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt

open Ck_sigs
open Ck_utils

let banner_path = ["config"; "banner"]

module Make(Clock : Ck_clock.S)
           (Git : Git_storage_s.S)
           (G : GUI_DATA)
           (RPC : RPC) = struct
  module R = Ck_rev.Make(Git)
  module Node = R.Node
  module Up = Ck_update.Make(Git)(Clock)(R)
  module Slow_history = Slow_set.Make(Clock)(Git_storage_s.Log_entry)(Git_storage_s.Log_entry_map)
  module Slow_log_entry = struct
    type t = Git_storage_s.Log_entry.t Slow_set.item
    let equal a b = Git_storage_s.Log_entry.equal (Slow_set.data a) (Slow_set.data b)
  end
  module Delta_history = Delta_RList.Make(Git_storage_s.Log_entry)(Slow_log_entry)(Git_storage_s.Log_entry_map)
  module Client = Ck_client.Make(Clock)(Git)(RPC)

  type gui_data = G.t

  module TreeNode = struct
    type group = int * string    (* int is the sort order *)
    let group_label (_, s) = s

    module Id_map = Ck_id.M
    module Sort_key = struct
      type t =
        | Item of Sort_key.t
        | ItemGroup of Sort_key.t
        | Group of group
      module Id = struct
        type t =
          | Item of Ck_id.t
          | Group of group
        let compare = compare
      end
      let compare a b =
        match a, b with
        | Item a, Item b -> Sort_key.compare a b
        | Item _, _ -> -1
        | _, Item _ -> 1
        (* Items come before groups for non-indented lists *)
        | ItemGroup a, ItemGroup b -> Sort_key.compare a b
        | ItemGroup _, Group _ -> 1
        | Group _, ItemGroup _ -> -1
        | Group a, Group b -> compare a b
      let show = function
        | Item a | ItemGroup a -> Sort_key.show a
        | Group (_, s) -> s
      let id = function
        | Item a | ItemGroup a -> Id.Item (Sort_key.id a)
        | Group s -> Id.Group s
    end
    module Child_map = Map.Make(Sort_key)

    module Item = struct
      include Node
      let show = name
      let id = Node.uuid
    end

    open Item.Types
    type adder =
      | Add_file of dir option

    type t = {
      item :
        [ `UniqueItem of Item.generic   (* ID is unique in tree *)
        | `GroupItem of Item.generic    (* ID is unique within parent *)
        | `Group of group ];            (* Label is unique within parent *)
      children : t Child_map.t;
      adder : adder option;
    }

    let sort_key t =
      match t.item with
      | `UniqueItem i -> Sort_key.Item (R.Node.key i)
      | `GroupItem i -> Sort_key.ItemGroup (R.Node.key i)
      | `Group g -> Sort_key.Group g

    let add item map =
      map |> Child_map.add (sort_key item) item

    let or_existing parent item =
      try Child_map.find (sort_key item) parent
      with Not_found -> item

    (* Add [/group*/child] to [top]
     * If any group (with the same key) already exists, add to that instead.
     * Since groups are only used as templates if the group is missing, they
     * must not contain children. *)
    let add_grouped ~top ~groups child =
      let rec aux top child = function
        | [] -> top |> add child
        | None :: gs -> aux top child gs
        | Some g :: gs ->
            assert (Child_map.is_empty g.children);
            let g = g |> or_existing top in
            let g = {g with children = aux g.children child gs} in
            top |> add g in
      top := aux !top child groups

    let item t =
      match t.item with
      | `UniqueItem node -> `UniqueItem (Item.id node, node)
      | `GroupItem node -> `GroupItem (Item.id node, node)
      | `Group _ as g -> g

    let children t = t.children
    let adder t = t.adder

    let group ~pri ?(children=Child_map.empty) ?adder label = {
      item = `Group (pri, label);
      children;
      adder;
    }

    let unique_of_node ?(children=Child_map.empty) ?adder n = {
      item = `UniqueItem (n :> Item.generic);
      children;
      adder;
    }
  end
  module WidgetTree = Reactive_tree.Make(Clock)(TreeNode)(G)

  module Item = TreeNode.Item
  module Widget = WidgetTree.Widget
  open Item.Types

  type review_mode = [ `Done | `Waiting | `Future | `Areas | `Everything ]

  type tree_view = Widget.t ReactiveData.RList.t

  type details = {
    details_item : Item.generic option React.S.t;
    details_parent : [ dir | file ] option React.S.t;
    details_children : Widget.t ReactiveData.RList.t;
    details_stop : stop;
  }

  type t = {
    repo : Git.Repository.t;
    master : Up.t;
    mutable r : R.t;
    tree : tree_view;
    mutable update_log : (?step:React.step -> Git_storage_s.Log_entry.t Git_storage_s.Log_entry_map.t -> unit) option;
    fixed_head : Git_storage_s.Log_entry.t option React.S.t;
    set_fixed_head : Git_storage_s.Log_entry.t option -> unit;
    mutable details : (details * (R.t -> unit)) Ck_id.M.t;
    mutable update_tree : R.t -> unit;
    mutable keep_me : unit React.S.t list;
    mutable review_mode : review_mode;
    client : Client.t option;
    server_head : Irmin.Hash.SHA1.t option React.S.t;
    banner : string React.S.t;
    set_banner : string -> unit;
  }

  module X : sig
    val freshen : t -> ([< Node.generic] as 'a) -> 'a
  end = struct
    let freshen t node = Obj.magic @@
      let uuid = Node.uuid node in
      match node with
      | `Dir _ | `File _ as node ->
          begin match R.get t.r uuid with
          | Some current when Node.equal current node -> (current :> Node.generic)
          | _ -> node end
  end
  let freshen = X.freshen

  let assume_changed _ _ = false

  let resolve ~base path =
    if String.length path > 0 && path.[0] = '/' then Irmin.Path.String_list.of_hum path
    else Irmin.Path.String_list.of_hum (Irmin.Path.String_list.to_hum base ^ "/" ^ path)

  let add maker t ?parent ~name ?(description="") () =
    let parent =
      match parent with
      | None -> Ck_id.root
      | Some p -> R.Node.uuid p in
    let disk_node = maker ~description in
    let path = resolve ~base:parent name in
    Up.add t.master ~base:t.r ~path disk_node >|= fun () ->
    R.get t.r path

  let add_file t =
    add Ck_disk_node.make_file t

  let add_child t parent name =
    match parent with
    | `Dir _ as p -> add_file t ~parent:p ~name ~description:"" ()

  let apply_adder t adder name =
    let ok x = (x :> Item.generic option) in
    match adder with
    | TreeNode.Add_file parent -> add_file t ?parent ~name () >|= ok

  let delete t node =
    Up.delete t.master [freshen t node]

  let set_name t item name =
    Up.set_name t.master (freshen t item) name

  let set_description t item v =
    Up.set_description t.master (freshen t item) v

  let make_process_tree r =
    let rec aux items =
      M.fold (fun _key item acc ->
        match item with
        | `File _ -> acc |> TreeNode.add (TreeNode.unique_of_node item)
        | `Dir _ as item -> add_group (TreeNode.Add_file (Some item)) item acc
      ) items TreeNode.Child_map.empty
    and add_group adder item acc =
      let children = aux (R.child_nodes item) in
      acc |> TreeNode.add (TreeNode.unique_of_node ~adder ~children item) in
    aux (R.roots r)

  let opt_node_equal a b =
    match a, b with
    | None, None -> true
    | Some a, Some b -> R.Node.equal (a :> Node.generic) (b :> Node.generic)
    | _ -> false

  let group_by_type ~parent child_nodes =
    let tree_nodes = ref TreeNode.Child_map.empty in
    let group_for node =
      let open TreeNode in
      match parent, node with
      | `File _, _ -> group ~pri:(-1) "Invalid"
      | `Dir _, `Dir _ -> group ~pri:1 "Subdirectories"
      | (`Dir _ as parent), `File _ ->
          let adder = Add_file (Some parent) in
          group ~pri:2 "Files" ~adder in
    let add node =
      TreeNode.add_grouped ~top:tree_nodes
        ~groups:[Some (group_for node)]
        (TreeNode.unique_of_node node) in
    child_nodes |> M.iter (fun _k v -> add v);
    !tree_nodes

  let empty_details =
    {
      details_item = React.S.const None;
      details_parent = React.S.const None;
      details_children = ReactiveData.RList.empty;
      details_stop = ignore
    }

  let on_remove r = function
    | `Dir _ | `File _ as node ->
        (R.get r (Node.uuid node) :> Node.generic option)

  let apa_details ~details_stop rs initial_node =
    let uuid = R.Node.uuid initial_node in
    let child_nodes node =
      R.child_nodes node
      |> group_by_type ~parent:node in
    let tree = WidgetTree.make (child_nodes initial_node) in
    let all = rs |> React.S.map ~eq:assume_changed (fun r ->
      let item = R.get r uuid in
      let item, parent, children =
        match item with
        | None -> None, None, TreeNode.Child_map.empty
        | Some item ->
            Some (item :> Item.generic), R.parent r item, child_nodes item in
      WidgetTree.update tree children ~on_remove:(on_remove r);
      (item, parent)
    ) in
    {
      details_item    = (all |> React.S.map ~eq:opt_node_equal (fun (item, _parent) -> item));
      details_parent  = (all |> React.S.map ~eq:opt_node_equal (fun (_item, parent) -> parent));
      details_children = WidgetTree.widgets tree;
      details_stop;
    }

  let details t initial_node =
    let initial_node = (initial_node :> Node.generic) in
    let uuid = Node.uuid initial_node in
    try fst (Ck_id.M.find uuid t.details)
    with Not_found ->
      let details_stop () =
        t.details <- t.details |> Ck_id.M.remove uuid in
      (* Note: initial_node may already be out-of-date *)
      let (>|>) = function
        | None -> fun _ -> empty_details, ignore
        | Some x -> fun f ->
            let rs, set_r = React.S.create ~eq:R.equal t.r in
            f ~details_stop rs x, (fun r -> set_r r) in
      let details, update =
        match initial_node with
        | `Dir _ | `File _ -> R.get t.r uuid >|> apa_details in
      t.details <- t.details |> Ck_id.M.add uuid (details, update);
      details

  type candidate = string * (unit -> unit Lwt.t)
  let candidate_label = fst
  let choose_candidate (_, set) = set ()

  let candidate_parents_for_pa t item =
    let item_uuid = Node.uuid item in
    let results = ref ["(no parent)", fun () -> Up.remove_parent t.master item] in
    let rec scan ~indent nodes =
      nodes |> M.iter (fun key node ->
        if Sort_key.id key <> item_uuid then (
          match node with
          | `Dir _ as node ->
              let path = R.Node.uuid node in
              results := (indent ^ Node.name node, fun () -> Up.move_into t.master item path) :: !results;
              R.child_nodes node |> scan ~indent:(indent ^ "» ")
          | `File _ -> ()
        )
      ) in
    R.roots t.r |> scan ~indent:"";
    List.rev !results

  let candidate_parents_for t item =
    let item = freshen t item in
    match item with
    | `Dir _ | `File _ as node -> candidate_parents_for_pa t node

  let rtree r fn =
    let rtree = WidgetTree.make (fn r) in
    let update_tree r =
      let on_remove node = (R.get r (Node.uuid node) :> Node.generic option) in
      WidgetTree.update rtree (fn r) ~on_remove in
    let widgets = WidgetTree.widgets rtree in
    (widgets, update_tree)

  let get_log master =
    Up.branch_head master
    |> Git.Commit.history ~depth:100

  let make_tree r = rtree r make_process_tree

  let tree t = t.tree
  let fix_head t entry =
    t.set_fixed_head entry;
    match entry with
    | None -> Up.fix_head t.master None
    | Some entry ->
        Git.Repository.commit t.repo entry.Git_storage_s.Log_entry.id
        >>= Up.fix_head t.master

  let fixed_head t = t.fixed_head

  let init_new_repo ~did_init repo =
    Git.Repository.empty repo >>= fun staging ->
    Ck_init.file_list |> Lwt_list.iter_p (fun path ->
      match Ck_init.read path with
      | None -> assert false
      | Some value ->
          let key = Irmin.Path.String_list.of_hum path in
          Git.Staging.update staging key value
    ) >>= fun () ->
    Git.Commit.commit staging ~msg:["Initialise repository"] >|= fun commit ->
    did_init := true;
    commit

  let init_repo ~did_init ?server ~server_branch repo =
    match server with
    | None -> init_new_repo ~did_init repo
    | Some base ->
        Client.fetch ~base ~server_branch >>= function
        | `Ok None -> init_new_repo ~did_init repo        (* Server is empty *)
        | `Ok (Some commit) -> return commit
        | `Cancelled_by_user -> failwith "Initial clone cancelled by user. Refresh to retry."
        | `Error msg -> failwith (Printf.sprintf "Failed to clone remote repository: %s" msg)

  let log_lock = Lwt_mutex.create ()

  let server_head t = t.server_head

  let enable_log t =
    assert (t.update_log = None);
    let log, set_log = React.S.create Git_storage_s.Log_entry_map.empty in
    t.update_log <- Some set_log;
    Lwt_mutex.with_lock log_lock (fun () ->
      get_log t.master >|= fun initial_log ->
      set_log initial_log;
      Slow_history.make ~delay:1.0 ~eq:Git_storage_s.Log_entry.equal log
      |> Delta_history.make
    )

  let disable_log t =
    match t.update_log with
    | None -> assert false
    | Some set_log ->
        set_log Git_storage_s.Log_entry_map.empty;
        t.update_log <- None

  let revert t entry =
    Up.revert t.master entry

  let read_banner r =
    match R.get r banner_path with
    | None -> "Untitled"
    | Some (`Dir _) -> "Banner is a directory!"
    | Some (`File _ as f) -> R.Node.description f

  let make ?(branch="master") ?server repo =
    let did_init = ref false in
    let on_update, set_on_update = Lwt.wait () in
    Git.Repository.branch repo Ck_client.tracking_branch >>= fun server_branch ->
    Git.Repository.branch ~if_new:(lazy (init_repo ~did_init ?server ~server_branch repo)) repo branch >>= fun master_branch ->
    Up.make ~repo ~on_update master_branch >>= fun master ->
    let server_head = Git.Branch.head server_branch >|~= function
      | None -> None
      | Some commit -> Some (Git.Commit.id commit) in
    let r = Up.head master in
    let fixed_head, set_fixed_head = React.S.create None in
    let rtree, update_tree = make_tree r in
    let client =
      match server with
      | None -> None
      | Some url ->
          let merge_from c = Up.sync master ~from:c in
          Some (Client.make ~master:master_branch ~server_branch ~merge_from url) in
    let banner, set_banner = React.S.create (read_banner r) in
    let t = {
      repo; master; r;
      tree = rtree; update_tree;
      update_log = None;
      fixed_head; set_fixed_head;
      details = Ck_id.M.empty;
      review_mode = `Done;
      keep_me = [];
      client;
      server_head;
      banner; set_banner;
    } in
    Lwt.wakeup set_on_update (fun r ->
      t.r <- r;
      t.set_banner (read_banner r);
      t.details |> Ck_id.M.iter (fun _id (_, set) -> set r);
      t.update_tree r;
      if not (Up.fixed_head t.master) then set_fixed_head None;
      match t.update_log with
      | None -> return ()
      | Some set_log ->
          (* If we're still processing an [enable_log], finish that first *)
          Lwt_mutex.with_lock log_lock (fun () ->
            get_log master >|= set_log
          )
    );
    begin match client with
    | Some client when !did_init ->
        begin Client.sync client >|= function
        | `Ok () | `Cancelled_by_user -> ()
        | `Error msg -> failwith (Printf.sprintf "Initialised new repository, but failed to push changes: %s" msg)
        end
    | _ -> return ()
    end >>= fun () ->
    return t

  let export_tar t =
    Up.head t.master |> R.commit |> Git.Commit.export_tar

  let client t = t.client
  let banner t = t.banner
end
