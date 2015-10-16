(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Ck_utils

type stop = unit -> unit
type 'a or_error = [ `Ok of 'a | `Error of string ]

module type DISK_NODE = sig
  module Types : sig
    type action_node
    type project_node

    type file = [`File of action_node]
    type dir = [`Dir of project_node]
  end
  open Types

  type generic = [ dir | file ]

  val description : file -> string
  val conflicts : [< generic ] -> string list
end

module type EQ = sig
  type t
  val equal : t -> t -> bool
end

module type TREE_MODEL = sig
  module Sort_key : Slow_set.SORT_KEY

  module Item : sig
    (** The data part of a node (excluding the child nodes).
     * This is passed through. *)
    type generic
    val equal : generic -> generic -> bool
    val show : generic -> string
  end

  module Child_map : Map.S with type key = Sort_key.t
  (** Ordered list of child nodes. *)

  type t
  type group
  type adder
  val group_label : group -> string
  val adder : t -> adder option
  val item : t ->
    [ `UniqueItem of Ck_id.t * Item.generic   (* ID is unique in tree *)
    | `GroupItem of Ck_id.t * Item.generic    (* ID is unique within parent *)
    | `Group of group ]                       (* Label is unique within parent *)
  val children : t -> t Child_map.t
end

module type GUI_DATA = sig
  type t
  (** For extra data the GUI wants to attach to tree nodes. *)
end

type problem = [ `Unread_conflicts ]

module type REV = sig
  type t

  module Node : sig
    include DISK_NODE

    val name : [< generic ] -> string

    val parent : [< generic ] -> Ck_id.t
    val with_parent : Ck_id.t -> [< generic ] -> generic

    val rev : [< generic] -> t

    val uuid : [< generic ] -> Ck_id.t

    val key : [< generic ] -> Sort_key.t
    (** A key for sorting by name. *)

    val equal : [< generic] -> [< generic] -> bool
    (** Note that the rev field is ignored, so nodes from different commits can
     * be equal. *)
  end
  open Node.Types

  type commit

  val equal : t -> t -> bool
  val child_nodes : [< dir | file ] -> [ dir | file ] M.t

  val roots : t -> [ dir | file ] M.t
  val commit : t -> commit

  val nodes : t -> [ dir | file] Ck_id.M.t

  val get : t -> Ck_id.t -> [ dir | file ] option

  val parent : t -> [< dir | file] -> [ dir | file ] option

  val problems : t -> (Node.generic * problem) list
  (** A list of nodes and problems to report. *)

  val alert : t -> bool
  (** Alert the user that file is required.
   * Currently, this is true when [problems t] is non-empty. *)
end

type 'a or_cancelled =
  [ `Ok of 'a
  | `Cancelled_by_user ]

type 'a or_error_or_cancelled =
  [ `Ok of 'a
  | `Error of string
  | `Cancelled_by_user ]

module type RPC = sig
  open Cohttp

  val get :
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) or_cancelled Lwt.t

  val post :
    ?body:Cohttp_lwt_body.t ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) or_cancelled Lwt.t
end
