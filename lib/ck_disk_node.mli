(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** The data that gets stored to disk, but not data we calculate on loading
 * (e.g. list of children). *)

open Ck_sigs

include DISK_NODE
open Types

val of_string : string -> [ dir | file ]
val to_string : [< dir | file ] -> string

val equal : ([< generic] as 'a) -> 'a -> bool

val make_file : description:string -> unit -> [> file]
val make_dir : unit -> [> dir]

val with_description : generic -> string -> generic

val as_dir : file -> dir
val as_file : dir -> file

val merge : ?base:[< dir | file] -> theirs:[< dir | file] -> [< dir | file] ->
  [dir | file]
val with_conflict : string -> ([< generic] as 'a) -> 'a
val without_conflicts : ([< generic] as 'a) -> 'a
