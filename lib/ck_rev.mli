(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** A single revision in the Irmin history. *)

open Ck_sigs

module type S = sig
  include REV
  open Node.Types

  val make : commit -> t Lwt.t
  val action_node : file -> string
end

module Make(Git : Git_storage_s.S) : S with type commit = Git.Commit.t
