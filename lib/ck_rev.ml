(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt

open Ck_utils

module type S = sig
  include Ck_sigs.REV
  open Node.Types

  val make : commit -> t Lwt.t
  val action_node : file -> string
end

let re_conflict = Regexp.regexp_with_flag "^<<<<<<< " "m"

module Make(Git : Git_storage_s.S) = struct
  type commit = Git.Commit.t

  module Node = struct
    module Types = struct
      type rev = {
        commit : Git.Commit.t;
        mutable roots : apa M.t;
        apa_nodes : apa Ck_id.M.t ref;
        mutable alert : bool;
        mutable problems :
          ( [ `File of action_node
            | `Dir of project_node ] * Ck_sigs.problem
          ) list;
      }
      and 'a node_details = {
        rev : rev;
        parent : Ck_id.t;
        children : apa M.t;
        uuid : Ck_id.t;
        disk_node : 'a;
      }
      and action_node = string node_details
      and project_node = unit node_details
      and apa =
        [ `File of action_node
        | `Dir of project_node ]
      and file = [`File of action_node]
      and dir = [`Dir of project_node]
    end

    open Types

    type generic = apa

    let disk_node = function
      | `File n -> `File n.disk_node
      | `Dir n -> `Dir n.disk_node

    let details = function
      | `File n -> {n with disk_node = ()}
      | `Dir n -> {n with disk_node = ()}

    let rev n = (details n).rev
    let uuid n = (details n).uuid

    let parent t = (details t).parent
    let name t =
      match Irmin.Path.String_list.rdecons (uuid t) with
      | None -> ""
      | Some (_dirname, basename) -> basename
    let description (`File t) = t.disk_node
    let conflicts = function
      | `Dir _ -> []
      | `File f ->
          match Regexp.string_match re_conflict (f.disk_node) 0 with
          | None -> []
          | Some _conflict -> ["Conflict markers present"]

    let key node = (String.lowercase (name node), uuid node)

    let with_parent parent = function
      | `File n -> `File {n with parent}
      | `Dir n -> `Dir {n with parent}

    let equal a b =
      let a = (a :> generic) in
      let b = (b :> generic) in
      uuid a = uuid b &&
      disk_node a = disk_node b
  end

  open Node.Types

  type rev = Node.Types.rev
  type t = rev

  let equal a b =
    Git.Commit.equal a.commit b.commit

  let child_nodes node = (Node.details node).children

  let roots t = t.roots

  let rec index ~apa_nodes items =
    items |> M.iter (fun (_, key) value ->
      apa_nodes := !apa_nodes |> Ck_id.M.add key value;
      index ~apa_nodes (Node.details value).children
    )

  let make_no_cache commit =
    Git.Commit.checkout commit >>= fun tree ->
    let apa_nodes = ref Ck_id.M.empty in
    let t = {
      commit; apa_nodes; roots = M.empty;
      alert = false;
      problems = [];
    } in
    let rec scan parent =
      Git.Staging.list tree parent >>= fun items ->
      Lwt_list.fold_left_s (fun acc path ->
        let uuid = path in
        let node =
          Git.Staging.read tree path >>= function
          | None ->
              scan path >>= fun children ->
              let disk_node = () in
              `Dir {rev = t; parent; children; uuid; disk_node} |> return
          | Some disk_node ->
              let children = M.empty in
              `File {rev = t; parent; uuid; disk_node; children} |> return in
        node >>= fun node ->
        acc |> M.add (Node.key node) node |> return
      ) M.empty items
    in
    scan Ck_id.root >>= fun roots ->
    index ~apa_nodes roots;
    if t.problems <> [] then t.alert <- true;
    t.roots <- roots;
    return t

  let last = ref None
  let make commit =
    let reload () =
      make_no_cache commit >|= fun r ->
      last := Some r;
      r in
    match !last with
    | Some last when Git.Commit.equal last.commit commit -> return last
    | _ -> reload ()

  let nodes t = !(t.apa_nodes)

  let get t uuid =
    try Some (Ck_id.M.find uuid !(t.apa_nodes))
    with Not_found -> None

  let parent t node = get t (Node.parent node)

  let commit t = t.commit

  let action_node (`File n) = n.disk_node

  let alert t = t.alert

  let problems t = List.rev (t.problems)
end
