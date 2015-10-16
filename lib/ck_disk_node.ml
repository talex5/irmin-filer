(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Sexplib.Conv
open Ck_utils

type node_details = {
  description : string;
  conflicts : string sexp_list;
} with sexp

type apa =
  [ `File of node_details
  | `Dir of node_details ]
  with sexp

type generic = apa

module Types = struct
  type action_node = node_details
  type project_node = node_details

  type file = [`File of action_node]
  type dir = [`Dir of project_node]
end

let details = function
  | `File d -> d
  | `Dir d -> d

let description t = (details t).description
let conflicts t = (details t).conflicts

let of_string s = `File {description = s; conflicts = []}
let to_string t = (details t).description

let make ~description = {
  description;
  conflicts = [];
}

let map_apa fn = function
  | `File d -> `File (fn d)
  | `Dir d -> `Dir (fn d)

let map_details fn = function
  | `File _ | `Dir _ as node -> map_apa fn node

let with_description node description = node |> map_details (fun d -> {d with description})
let equal = (=)

let make_file ~description () =
  `File (make ~description)

let make_dir () =
  `Dir (make ~description:"")

let merge_description ~log ~base ~theirs ours =
  if base = theirs then ours
  else if base = ours then theirs
  else if theirs = ours then theirs
  else (
    log "Conflicting descriptions; keeping both";
    List.sort String.compare [ours; theirs]
    |> String.concat "\n\n----\n\n"
  )

(* Used for the (unlikely) case of a merge with no common ancestor *)
let default_base = make ~description:""

let dedup xs =
  let rec aux acc = function
    | [] -> acc
    | x :: ((y :: _) as rest) when x = y -> aux acc rest
    | x :: xs -> aux (x :: acc) xs in
  aux [] (List.sort String.compare xs)

let merge_details ~log ~base ~theirs ours =
  let {description; conflicts} = ours in
  let description = merge_description ~log ~base:base.description ~theirs:theirs.description description in
  let conflicts   = dedup (conflicts @ theirs.conflicts) in
  {description; conflicts}

let merge_dir ~log ~base ~theirs ours =
  let `Dir base_details = base in
  let their_details = theirs in
  `Dir (merge_details ~log ~base:base_details ~theirs:their_details ours)

let merge_file ~log ~base ~theirs ours =
  let `File base_details = base in
  let their_details = theirs in
  let details =
    merge_details ~log ~base:base_details ~theirs:their_details ours in
  `File details

let as_dir = function
  | `File d -> `Dir d
  | `Dir _ as p -> p

let as_file = function
  | `Dir d -> `File d
  | `File _ as a -> a

let merge ?base ~theirs ours =
  let base = (base :> apa option) |> default (`Dir default_base) in
  let theirs = (theirs :> apa) in
  let ours = (ours :> apa) in
  if base = theirs then ours
  else if base = ours then theirs
  else (
    let conflicts = ref [] in
    let log msg = conflicts := msg :: !conflicts in
    let merged =
      match theirs, ours with
      | `Dir theirs, `Dir ours -> merge_dir ~log ~base:(as_dir base) ~theirs ours
      | `File theirs, `File ours -> merge_file ~log ~base:(as_file base) ~theirs ours
      | theirs, ours ->
          log "Type mismatch: converting to dir";
          let `Dir theirs = as_dir theirs in
          let `Dir ours = as_dir ours in
          merge_dir ~log ~base:(as_dir base) ~theirs ours
    in
    merged |> map_apa (fun d -> {d with conflicts = d.conflicts @ !conflicts})
  )

let with_conflict msg node = node |> map_details (fun d -> {d with conflicts = msg :: d.conflicts})
let with_conflict : string -> ([< generic] as 'a) -> 'a = Obj.magic with_conflict

let without_conflicts node = node |> map_details (fun d -> {d with conflicts = []})
let without_conflicts : ([< generic] as 'a) -> 'a = Obj.magic without_conflicts
