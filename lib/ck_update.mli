(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

(** Making changes to the store. *)

open Ck_sigs

module Make(Git : Git_storage_s.S)
           (Clock : Ck_clock.S)
           (R : Ck_rev.S with type commit = Git.Commit.t) : sig
  type t
  type update_cb = R.t -> unit Lwt.t

  open R.Node.Types

  val make : repo:Git.Repository.t -> on_update:update_cb Lwt.t -> Git.Branch.t -> t Lwt.t
  (** Manage updates to this branch.
   * Calls [on_update] after the branch has changed (either due to the methods below or because
   * the store has been modified by another process. *)

  val head : t -> R.t
  (** The current head. Usually the branch tip, but can be different if [fix_head] is used.
   * Also, this is the cached version of the last state of the head. It is the version
   * passed to [on_update] and might lag the real head slightly. *)

  val fix_head : t -> Git.Commit.t option -> unit Lwt.t
  (** Set [head] to the given commit and pause tracking our branch.
   * Pass [None] to return to tracking the branch's head.
   * Modifications made via [t] will automatically resume tracking, but changes
   * made by other means will be ignored. *)

  val fixed_head : t -> bool

  val branch_head : t -> Git.Commit.t
  (** The current tip of the branch (whatever the setting of [fix_head]) *)

  (** Functions for making updates all work in the same way.
   * 1. Make a new branch from the commit that produced the source item.
   * 2. Commit the change to that branch (this should always succeed).
   * 3. Merge the new branch to master.
   * 4. Call the [on_update] function.
   * When they return, on_update has completed for the new revision. *)

  val add : t ->
    base:R.t ->
    path:Ck_id.t ->
    (unit -> [ Ck_disk_node.Types.dir | Ck_disk_node.Types.file]) ->
    unit Lwt.t
  val delete : t -> ?msg:string -> [< R.Node.generic] list -> unit or_error Lwt.t

  val set_name : t -> [< R.Node.generic ] -> Ck_id.t -> unit Lwt.t
  val set_description : t -> R.Node.Types.file -> string -> unit Lwt.t

  val move_into : t -> [< dir | file] -> Ck_id.t -> unit Lwt.t
  val remove_parent : t -> [< dir | file] -> unit Lwt.t

  val revert : t -> Git_storage_s.Log_entry.t -> unit or_error Lwt.t

  val sync : t -> from:Git.Commit.t -> unit or_error Lwt.t
end
