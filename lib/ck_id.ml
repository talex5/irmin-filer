(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

type t = Irmin.Path.String_list.t

let root = []

let to_string = Irmin.Path.String_list.to_hum
let of_string = Irmin.Path.String_list.of_hum
let fmt () = to_string
let compare = Irmin.Path.String_list.compare

module M = Map.Make(Irmin.Path.String_list)
module S = Set.Make(Irmin.Path.String_list)
