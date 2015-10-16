(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Ck_utils
open Lwt

let async : (unit -> unit Lwt.t) -> unit = Lwt.async

module Make(Git : Git_storage_s.S) (Clock : Ck_clock.S) (R : Ck_rev.S with type commit = Git.Commit.t) = struct
  module Merge = Ck_merge.Make(Git)(R)

  type t = {
    repo : Git.Repository.t;
    branch : Git.Branch.t;
    mutable fixed_head : float option;       (* If [Some time], we are in "time-machine" mode, and not tracking [branch] *)
    mutable head : R.t;
    updated : unit Lwt_condition.t;
    mutex : Lwt_mutex.t;
    mutable update_signal : unit React.S.t;
    on_update : (R.t -> unit Lwt.t) Lwt.t;   (* (a thread just to avoid a cycle at creation time) *)
  }

  let update_head t new_head =   (* Call with mutex locked *)
    R.make new_head >>= fun new_head ->
    t.head <- new_head;
    t.on_update >>= fun on_update ->
    on_update new_head >|= fun () ->
    Lwt_condition.broadcast t.updated ()

  type update_cb = R.t -> unit Lwt.t

  (* Must be called with t.mutex held *)
  let maybe_update_head t new_head =
    let old_head = R.commit t.head in
    match new_head with
    | None -> failwith "Branch has been deleted!"
    | Some new_head when Git.Commit.equal old_head new_head -> return ()
    | Some new_head -> update_head t new_head

  let make ~repo ~on_update branch =
    let mutex = Lwt_mutex.create () in
    match Git.Branch.head branch |> React.S.value with
    | None -> failwith "No commits on branch!"
    | Some initial_head ->
    R.make initial_head >>= fun initial_head ->
    let updated = Lwt_condition.create () in
    let update_scheduled = ref false in
    let t = {
      repo;
      branch;
      fixed_head = None;
      head = initial_head;
      updated;
      mutex;
      update_signal = React.S.const ();
      on_update;
    } in
    t.update_signal <-
      Git.Branch.head branch |> React.S.map (fun _ ->
        if not (!update_scheduled) then (
          update_scheduled := true;
          async (fun () ->
            Lwt_mutex.with_lock mutex (fun () ->
              update_scheduled := false;
              if t.fixed_head = None then (
                (* Head might have changed while we waited for the lock. *)
                React.S.value (Git.Branch.head branch)
                |> maybe_update_head t
              ) else return ()  (* Fixed head - ignore updates *)
            )
          )
        )
      );
    return t

  let fix_head t = function
    | None ->
        Lwt_mutex.with_lock t.mutex (fun () ->
          t.fixed_head <- None;
          React.S.value (Git.Branch.head t.branch)
          |> maybe_update_head t
        )
    | Some commit as new_head ->
        Lwt_mutex.with_lock t.mutex (fun () ->
          Git.Commit.task commit >>= fun task ->
          let time = Irmin.Task.date task |> Int64.to_float in
          t.fixed_head <- Some time;
          maybe_update_head t new_head
        )

  let head t = t.head
  let fixed_head t = t.fixed_head <> None

  let branch_head t =
    match React.S.value (Git.Branch.head t.branch) with
    | None -> failwith "Branch has been deleted!"
    | Some commit -> commit

  let mem uuid rev =
    R.get rev uuid <> None

  let ff_master t commit =
    (* Check that the commit is readable *)
    Lwt.catch (fun () -> R.make commit >|= ignore)
      (fun ex -> bug "Change generated an invalid commit:\n%s\n\nThis is a BUG. The invalid change has been discarded."
        (Printexc.to_string ex)) >>= fun () ->
    Lwt_mutex.with_lock t.mutex (fun () ->
      if R.commit t.head |> Git.Commit.equal commit then
        return (`Ok, return ())
      else (
        (* At this point, head cannot contain our commit because we haven't merged it yet,
         * and no updates can happen while we hold the lock. *)
        let updated = Lwt_condition.wait t.updated in
        Git.Branch.fast_forward_to t.branch commit >|= fun merge_result ->
        (* If `Ok, [updated] cannot have fired yet because we still hold the lock. When it does
         * fire next, it must contain our update. It must fire soon, as head has changed. *)
        if merge_result = `Ok then (
          (* If we were on a fixed head then return to tracking master. Otherwise, the user won't
           * see the update. *)
          t.fixed_head <- None;
        );
        (merge_result, updated)
      )
    )

  (* Branch from base, apply [fn branch] to it, then merge the result back to master.
   * Returns only once [on_update] has been run for the new revision. *)
  let merge_to_master t ~base ~msg fn =
    let base_commit = R.commit base in
    Git.Commit.checkout base_commit >>= fun view ->
    fn view >>= fun result ->
    Git.Commit.commit ~msg:[msg] view >>= fun pull_rq ->
    let rec aux () =
      (* Merge to branch tip, even if we're on a fixed head *)
      let old_head = branch_head t in
      Merge.merge ~repo:t.repo ~base:base_commit ~theirs:old_head pull_rq >>= function
      | `Nothing_to_do ->
          (* Our change had no effect, so there's nothing to do. *)
          return (return ())
      | `Ok merged ->
          ff_master t merged >>= function
          | `Ok, updated -> return updated
          | `Not_fast_forward, _updated ->
              Log.warn "Update while we were trying to merge - retrying...";
              Clock.sleep 1.0 >>= fun () ->      (* Might be a bug - avoid hanging the browser *)
              (* Possibly we should wait for branch_head to move, but this is a very unlikely case
               * so do a simple sleep-and-retry *)
              aux ()
      in
    aux () >>= fun updated ->     (* Changes have been committed. *)
    updated >>= fun () ->         (* [on_update] has been called. *)
    return result

  let revert t log_entry =
    Merge.revert ~repo:t.repo ~master:(branch_head t) log_entry >>= function
    | `Nothing_to_do -> return (`Ok ())
    | `Error _ as e -> return e
    | `Ok commit ->
        ff_master t commit >>= function
        | `Ok, updated -> updated >|= fun () -> `Ok ()
        | `Not_fast_forward, _updated ->
            return (error "Update while we were trying to revert - aborting")

  let sync t ~from:theirs =
    let ours = branch_head t in
    let ff commit =
      ff_master t commit >>= function
      | `Ok, updated -> updated >|= fun () -> `Ok ()
      | `Not_fast_forward, _updated ->
          return (error "Update while we were trying to sync - aborting") in
    let sync_with ~base =
      Merge.merge ~repo:t.repo ?base ~theirs ours >>= function
      | `Nothing_to_do -> return (`Ok ())
      | `Ok merge -> ff merge in
    Git.Commit.lcas ours theirs >>= function
    | [] -> sync_with ~base:None
    | lcas ->
        if List.exists (Git.Commit.equal ours) lcas then ff theirs   (* Trivial - we have no changes *)
        else sync_with ~base:(Some (List.hd lcas))

  let create t ~base ~path (node:[< Ck_disk_node.generic]) =
    assert (not (mem path base));
    let s = Ck_disk_node.to_string node in
    let msg = Printf.sprintf "Create %a" Ck_id.fmt path in
    merge_to_master t ~base ~msg (fun view ->
      Git.Staging.update view path s
    )

  let update t ~msg node new_contents =
    let base = R.Node.rev node in
    let uuid = R.Node.uuid node in
    merge_to_master t ~base ~msg (fun view ->
      assert (mem uuid base);
      Git.Staging.update view uuid new_contents
    )

  let move t ~base ~msg ~old_path ~new_path =
    merge_to_master t ~base ~msg (fun view ->
      Git.Staging.read_exn view old_path >>= Git.Staging.update view new_path >>= fun () ->
      Git.Staging.remove view old_path
    )

  let delete t ?msg nodes =
    match nodes with
    | [] -> return (`Ok ())
    | x :: _ ->
        let base = R.Node.rev x in
        let msg =
          match msg with
          | None ->
              nodes
              |> List.map R.Node.name
              |> String.concat ", "
              |> Printf.sprintf "%s: deleted"
          | Some msg -> msg in
        merge_to_master ~base ~msg t (fun view ->
          nodes |> Lwt_list.iter_s (fun n -> Git.Staging.remove view (R.Node.uuid n))
        ) >|= fun () ->
        `Ok ()

  let add t ~base ~path maker =
    let disk_node = maker () in
    create t ~base ~path disk_node

  let set_name t node new_path =
    let old_path = R.Node.uuid node in
    let msg = Printf.sprintf "%a: rename to %a" Ck_id.fmt old_path Ck_id.fmt new_path in
    let base = R.Node.rev node in
    move t ~msg ~base ~old_path ~new_path

  let set_description t node v =
    let msg = Printf.sprintf "%s: update description" (R.Node.name node) in
    update t ~msg node v

  let move_into t node parent_path =
    let base = R.Node.rev node in
    let old_path = R.Node.uuid node in
    match Irmin.Path.String_list.rdecons old_path with
    | None -> failwith "Can't rename the root!"
    | Some (_old_dirname, basename) ->
    let new_path = Irmin.Path.String_list.rcons parent_path basename in
    let msg = Printf.sprintf "%a: move to %a" Ck_id.fmt old_path Ck_id.fmt new_path in
    move t ~msg ~base ~old_path ~new_path

  let remove_parent t node =
    move_into t node Ck_id.root
end
