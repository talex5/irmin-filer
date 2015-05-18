(* Copyright (C) 2015, Thomas Leonard.
 * See the README file for details. *)

open Lwt

module IO = struct
  type in_channel = unit
  type out_channel = Buffer.t
  let really_input _ch _buf _pos _len = failwith "unused"
  let input = really_input
  let output = Buffer.add_substring
  let close_out _ = ()
end

module T = Tar.Make(IO)

module Make (I : Irmin.BASIC with type key = string list and type value = string) = struct
  module V = Irmin.View(I)

  (* This is a work-around for https://github.com/mirage/irmin/issues/204 *)
  module SliceIO = struct
    module Make_list (K: Tc.S0)(V: Tc.S0) = Tc.List( Tc.Pair(K)(V) )
    module Ct = Make_list(I.Private.Contents.Key)(Tc.String)
    module No = Make_list(I.Private.Node.Key)(I.Private.Node.Val)
    module Cm = Make_list(I.Private.Commit.Key)(I.Private.Commit.Val)
    module T = Tc.Triple (Ct)(No)(Cm)
  end

  module Bundle = Tc.Pair(SliceIO.T)(I.Head)

  type repo = {
    config : Irmin.config;
    task_maker : string -> Irmin.task;
    empty : string -> I.t;
  }

  module Staging = struct
    type t = {
      repo : repo;
      view : V.t;
    }

    let of_view repo view = {repo; view}
    let list t = V.list t.view
    let read_exn t = V.read_exn t.view
    let update t = V.update t.view
    let remove t = V.remove t.view
    let mem t = V.mem t.view
  end

  module Commit = struct
    type t = {
      repo : repo;
      store : string -> I.t;
    }

    type id = Irmin.Hash.SHA1.t

    module History = I.History

    let id t =
      match I.branch (t.store "get commit ID") with
      | `Tag _ | `Empty -> assert false
      | `Head id -> id

    let equal a b =
      id a = id b

    let of_id repo id =
      I.of_head repo.config repo.task_maker id >|= fun store ->
      {repo; store}

    let checkout t =
      V.of_path (t.store "Make view") I.Key.empty >|= Staging.of_view t.repo

    let commit ?parents staging ~msg =
      let repo = staging.Staging.repo in
      let parents =
        match parents with
        | Some parents -> parents |> List.map id
        | None -> V.parents staging.Staging.view in
      let task =
        match msg with
        | [] -> failwith "Empty commit message!"
        | [summary] -> repo.task_maker summary
        | summary :: body ->
            let t = repo.task_maker summary in
            body |> List.iter (Irmin.Task.add t);
            t in
      V.make_head (repo.empty (List.hd msg)) task ~parents ~contents:staging.Staging.view
      >>= I.of_head repo.config repo.task_maker
      >|= fun store -> { repo; store }

    let history ?depth t =
      let store = t.store "Read history" in
      let open Git_storage_s in
      let task_of_hash = Hashtbl.create 100 in
      let module Top = Graph.Topological.Make_stable(struct
        type t = I.History.t
        let in_degree = I.History.in_degree
        let iter_succ = I.History.iter_succ
        let iter_vertex = I.History.iter_vertex
        module V = struct
          include I.History.V
          let compare a b =
            let ta = Hashtbl.find task_of_hash a in
            let tb = Hashtbl.find task_of_hash b in
            match Int64.compare (Irmin.Task.date ta) (Irmin.Task.date tb) with
            | 0 -> I.History.V.compare a b
            | r -> r
        end
      end) in

      I.history ?depth store >>= fun history ->
      let hashes_needed = ref [] in
      (* Start fetching all commits in the history *)
      history |> I.History.iter_vertex (fun hash ->
        hashes_needed := hash :: !hashes_needed
      );
      (* Wait for them to complete and put in a hash table *)
      !hashes_needed |> Lwt_list.iter_s (fun hash ->
        I.task_of_head store hash >|= Hashtbl.add task_of_hash hash
      ) >>= fun () ->
      (* Set rank field according to topological order and build final result map *)
      let map = ref Log_entry_map.empty in
      let rank = ref 0 in
      history |> Top.iter (fun hash ->
        let task = Hashtbl.find task_of_hash hash in
        incr rank;
        let msg = Irmin.Task.messages task in
        let date = Irmin.Task.date task |> Int64.to_float in
        let entry = {Log_entry.date; rank = !rank; msg; id = hash} in
        map := !map |> Log_entry_map.add entry entry
      );
      return !map

    let merge a b =
      I.of_head a.repo.config a.repo.task_maker (id a) >>= fun tmp ->
      I.merge_head (tmp "Merge") (id b) >|= function
      | `Ok () -> `Ok {a with store = tmp}
      | `Conflict _ as c -> c

    let export_tar t =
      V.of_path (t.store "export_tar") I.Key.empty >>= fun v ->
      let buf = Buffer.create 10240 in
      let files = ref [] in
      let rec scan dir =
        V.list v dir >>=
        Lwt_list.iter_s (fun path ->
          V.read v path >>= function
          | None -> scan path
          | Some data ->
              let header = T.Header.make
                ~file_mode:0o644
                (String.concat "/" path) (String.length data |> Int64.of_int) in
              let write b = Buffer.add_string b data in
              files := (header, write) :: !files;
              return ()
        ) in
      scan [] >|= fun () ->
      T.Archive.create_gen (Stream.of_list !files) buf;
      Buffer.contents buf

    let bundle_create s ~basis head =
      I.export s ~min:basis ~max:[head] >|= fun slice ->
      let slice = I.Private.Slice.explode slice in
      let bundle = (slice, head) in
      let buf = Cstruct.create (Bundle.size_of bundle) in
      let rest = Bundle.write bundle buf in
      assert (Cstruct.len rest = 0);
      Some (Cstruct.to_string buf)

    let bundle ~tracking_branch commit =
      let head = id commit in
      I.of_tag commit.repo.config commit.repo.task_maker tracking_branch >>= fun s ->
      let s = s "bundle" in
      I.head s >>= function
      | Some old_head when old_head = head -> return None
      | Some old_head -> bundle_create s ~basis:[old_head] head
      | None -> bundle_create s ~basis:[] head

    let parents t =
      let head = id t in
      I.history ~depth:1 (t.store "parents") >>= fun history ->
      I.History.pred history head
      |> Lwt_list.map_s (of_id t.repo)

    let task t =
      I.task_of_head (t.store "task") (id t)

    let lcas t other =
      I.lcas "lcas" t.store other.store >>= function
      | `Ok ids -> ids |> Lwt_list.map_s (of_id t.repo)
      | `Max_depth_reached |`Too_many_lcas -> assert false  (* Can't happen *)

  end

  module Branch = struct
    type t = {
      repo : repo;
      store : string -> I.t;
      head_id : Commit.id option ref;
      head : Commit.t option React.S.t;
      unwatch : unit -> unit Lwt.t;
    }

    let opt_commit_equal a b =
      match a, b with
      | Some a, Some b -> Commit.equal a b
      | None, None -> true
      | _ -> false

    let of_store repo store ~if_new =
      let commit_of_id = function
        | None -> return None
        | Some id -> Commit.of_id repo id >|= fun commit -> Some commit in
      I.head (store "Get latest commit") >>= (function
        | Some id -> return id
        | None ->
            Lazy.force if_new >>= fun commit ->
            let new_id = Commit.id commit in
            I.compare_and_set_head (store "Initialise repository") ~test:None ~set:(Some new_id) >>= function
            | true -> return new_id
            | false ->
                Log.warn "Concurrent attempt to initialise new branch; discarding our attempt";
                I.head_exn (store "Read new head")
      ) >>= fun initial_head_id ->
      let head_id = ref (Some initial_head_id) in
      Commit.of_id repo initial_head_id >>= fun initial_head ->
      let head, set_head = React.S.create ~eq:opt_commit_equal (Some initial_head) in
      I.watch_head (store "Watch branch") ~init:initial_head_id (fun _diff ->
        (* (ignore the commit ID in the update message; we want the latest) *)
        I.head (store "Get latest commit") >>= fun new_head_id ->
        if new_head_id <> !head_id then (
          head_id := new_head_id;
          commit_of_id new_head_id >|= set_head
        ) else return ()
      ) >>= fun unwatch ->
      return {
        repo;
        store;
        head_id;
        head;
        unwatch;
      }

    let head t = t.head

    let fast_forward_to t commit =
      (* Note: can't use [I.fast_forward_head] because it can sometimes return [false] on success
       * (when already up-to-date). *)
      let store = t.store "Fast-forward" in
      let commit_id = Commit.id commit in
      let old_head = !(t.head_id) in
      let do_ff () =
        I.compare_and_set_head store ~test:old_head ~set:(Some commit_id) >|= function
        | true -> `Ok
        | false -> `Not_fast_forward in   (* (concurrent update) *)
      match old_head with
      | None -> do_ff ()
      | Some expected ->
          I.lcas_head store commit_id >>= function
          | `Ok lcas ->
              if List.mem expected lcas then do_ff ()
              else return `Not_fast_forward
          (* These shouldn't happen, because we didn't set any limits *)
          | `Max_depth_reached | `Too_many_lcas -> assert false

    let release t =
      t.unwatch ()
  end

  module Repository = struct
    type t = repo

    let branch t ~if_new name =
      I.of_tag t.config t.task_maker name >>= Branch.of_store t ~if_new

    let force_branch t branch commit =
      I.of_tag t.config t.task_maker branch >>= fun s ->
      match commit with
      | None -> I.remove_tag (s "delete branch") branch
      | Some commit -> I.update_head (s "force") (Commit.id commit)

    let branch_head t branch =
      I.of_tag t.config t.task_maker branch >>= fun s ->
      I.head (s "branch_head")

    let commit t hash =
      Commit.of_id t hash >>= fun c ->
      let commit_store = I.Private.commit_t (c.Commit.store "check commit exists") in
      I.Private.Commit.mem commit_store hash >|= function
      | true -> Some c
      | false -> None

    let empty t = V.empty () >|= Staging.of_view t

    let fetch_bundle t ~tracking_branch bundle =
      I.of_tag t.config t.task_maker tracking_branch >>= fun s ->
      let s = s "import" in
      let (slice, head) = Bundle.read (Mstruct.of_string bundle) in
      commit t head >>= function
      | Some c -> I.update_head s head >|= fun () -> `Ok c
      | None ->
      let slice = I.Private.Slice.implode slice in
      I.import s slice >>= fun () ->
      commit t head >>= function
      | None -> return (`Error "Head commit not found after importing bundle!")
      | Some head_commit ->
      I.update_head s head >|= fun () -> `Ok head_commit
  end

  let make config task_maker =
    begin match Bin_prot.Size.bin_size_int64 0x8000L with
    | 5 -> ()
    | x ->
        Ck_utils.bug
          "Bin_prot serialisation is broken (says it needs %d bytes to store 0x8000). \
          Ensure js_of_ocaml branch is pinned." x end;
    I.empty config task_maker >|= fun empty ->
    {config; task_maker; empty}
end
