(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Ck_sigs

module type MODEL = sig
  type t
  type gui_data

  type review_mode = [ `Done | `Waiting | `Future | `Areas | `Everything ]

  module Item : sig
    include DISK_NODE

    val name : [< generic ] -> string
    val uuid : [< generic] -> Ck_id.t
  end

  open Item.Types

  module Widget : sig
    (** An object visible on the screen. *)
    type t

    val item : t -> [
      | `Item of [ dir | file ] React.S.t
      | `Group of string
    ]
    val children : t -> t ReactiveData.RList.t
    val state : t -> Slow_set.state React.S.t
    val gui_data : t -> gui_data option ref
    val unique : t -> bool
    (** Unique items occur at most once in the tree (and are often leaves).
     * Non-unique items are used for grouping, and are typically rendered as headings. *)

    type adder
    val adder : t -> adder option
  end

  type details = {
    details_item : [ dir | file ] option React.S.t;
    details_parent : [ dir | file ] option React.S.t;
    details_children : Widget.t ReactiveData.RList.t;
    details_stop : stop;
  }

  val add_file : t -> ?parent:[< dir] ->
                   name:string -> ?description:string -> unit -> [dir | file] option Lwt.t

  val add_child : t -> [< dir ] -> string -> [dir | file] option Lwt.t
  val apply_adder : t -> Widget.adder -> string -> Item.generic option Lwt.t

  val delete : t -> [< Item.generic] -> unit or_error Lwt.t

  val set_name : t ->  [< Item.generic] -> Ck_id.t -> unit Lwt.t
  val set_description : t -> Item.Types.file -> string -> unit Lwt.t

  type candidate

  val candidate_parents_for : t -> [< dir | file] -> candidate list
  (** Get the possible new parents for an item. *)

  val candidate_label : candidate -> string
  val choose_candidate : candidate -> unit Lwt.t

  val server_head : t -> Irmin.Hash.SHA1.t option React.S.t
  (** The last commit we know the server has. *)

  val enable_log : t -> Git_storage_s.Log_entry.t Slow_set.item ReactiveData.RList.t Lwt.t
  val disable_log : t -> unit
  val revert : t -> Git_storage_s.Log_entry.t -> unit or_error Lwt.t

  val fix_head : t -> Git_storage_s.Log_entry.t option -> unit Lwt.t
  val fixed_head : t -> Git_storage_s.Log_entry.t option React.S.t

  val tree : t -> Widget.t ReactiveData.RList.t

  val details : t -> [< Item.generic] -> details

  val export_tar : t -> string Lwt.t
  (** Export the current state as a tar file *)

  module Client : sig
    type t

    val sync : t -> unit or_error_or_cancelled Lwt.t
    (** Sync with server. *)

    val sync_in_progress : t -> bool React.S.t
    (** True while we are syncing with the remote server. *)
  end

  val client : t -> Client.t option
  (** The client for syncing with our server, if any. *)

  val banner : t -> string React.S.t
  (** Title to display at the top. *)
end
