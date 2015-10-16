(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Lwt

module Make(Git : Git_storage_s.S) (R : Ck_rev.S with type commit = Git.Commit.t) = struct
  module PathMap = Map.Make(Irmin.Path.String_list)
  module Merge_files = Irmin.Merge.Map(PathMap)(Irmin.Path.String_list)

  let ok x = return (`Ok x)

  let as_map c =
    let m = ref PathMap.empty in
    Git.Commit.iter c (fun key value ->
      value >|= fun value ->
      m := !m |> PathMap.add key value
    ) >|= fun () ->
    !m

  (* Ensure we have a trailing EOL *)
  let with_eol x =
    let l = String.length x in
    if l > 0 && x.[l - 1] = '\n' then x
    else x ^ "\n"

  let merge_contents ~base ours theirs =
    ignore base;
    Printf.sprintf "\
    <<<<<<< ours\n\
    %s\
    =======\n\
    %s\
    >>>>>>> theirs\n" (with_eol ours) (with_eol theirs)

  let merge_file ~base path ours theirs =
    match ours, theirs with
    | Some x, Some y when x = y -> ours
    | _ ->
        let base = try Some (PathMap.find path base) with Not_found -> None in
        if base = ours then theirs
        else if base = theirs then ours
        else match base, ours, theirs with
        | None, Some ours, Some theirs -> Some (merge_contents ~base:"" ours theirs)   (* Double creation *)
        | Some base, Some ours, Some theirs -> Some (merge_contents ~base ours theirs) (* Double modification *)
        | Some _base, None, Some keep
        | Some _base, Some keep, None -> Some keep    (* One modified, one deleted *)
        | None, None, keep
        | None, keep, None -> keep                    (* One-side created *)
        | Some _, None, None -> None                  (* Double delete *)

  let stage_merge ~repo ?base ~theirs ours =
    (** Plan:
        1. Get all names in theirs.
        2. Union with names in ours.
        3. Pair-wise merge, adding conflict markers as needed.
        4. Check for directory/file conflicts and move into directories where needed.
        
        Don't use Irmin's merge functions because they can fail, and we want to make sure
        that can't happen. Also, not sure if they handle file/directory conflicts. *)

    as_map theirs >>= fun theirs ->
    as_map ours >>= fun ours ->
    begin match base with
    | None -> return PathMap.empty
    | Some base -> as_map base
    end >>= fun base ->
    let merged = PathMap.merge (merge_file ~base) ours theirs in
    Git.Repository.empty repo >>= fun stage ->
    (* TODO: file/directory conflicts *)
    PathMap.bindings merged |> Lwt_list.iter_s (fun (path, value) ->
      Git.Staging.update stage path value
    ) >|= fun () ->
    stage

  let merge ~repo ?base ~theirs ours =
    match base with
    | Some base when Git.Commit.equal base theirs -> ok ours  (* The common case *)
    | _ ->
    stage_merge ~repo ?base ~theirs ours >>= fun stage ->
    (* We could perhaps avoid a merge here if stage = theirs, but probably not worth it. *)
    Git.Commit.commit ~parents:[theirs; ours] stage ~msg:["Merge"] >>= ok

  let revert ~repo ~master log_entry =
    let open Git_storage_s in
    Git.Repository.commit repo log_entry.Log_entry.id >>= function
    | None -> return (`Error "Commit to revert does not exist!")
    | Some commit ->
    let orig_summary = match log_entry.Log_entry.msg with [] -> "-" | summary::_ -> summary in
    let msg = [
      Printf.sprintf "Revert \"%s\"" orig_summary;
      "";
      Printf.sprintf "This reverts commit %s." (Irmin.Hash.SHA1.to_hum log_entry.Log_entry.id)
    ] in
    Git.Commit.parents commit >>= function
    | [] -> return (`Error "Can't revert initial commit!")
    | _::_::_ -> return (`Error "Can't revert merges, sorry")
    | [parent] ->
    stage_merge ~repo ~base:commit ~theirs:master parent >>= fun stage ->
    Git.Commit.commit ~parents:[master] stage ~msg >>= ok
end
