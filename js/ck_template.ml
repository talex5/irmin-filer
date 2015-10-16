(* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. *)

open Tyxml_js
open Html5
open Ck_utils
open Ck_js_utils

open Lwt.Infix

module Gui_tree_data = struct
  (* If the gui_data for a widget is None then it has just appeared.
   * We set the value to the newly-created element.
   *
   * If an item appears with a value already set then this is a move.
   * We read the old data to get the old height (needed for the animation),
   * and then update it to point at the new element.
   *)
  type t = Dom_html.element Js.t
end

(* Cross-browser form submit buttons *)
let submit_button label =
  try
    input ~a:[a_input_type `Submit; a_value label] ()
  with _ ->
    (* Hello, MSIE!
     * http://reference.sitepoint.com/javascript/Element/setAttributeNode *)
    let s = span [] in
    let elem = Tyxml_js.To_dom.of_span s in
    elem##innerHTML <- Js.string (Printf.sprintf "<input type='submit' value='%s'>" label);
    s

let show_modal, modal_div =
  let dropdown, set_dropdown = ReactiveData.RList.make [] in
  let dropdown_style, set_dropdown_style = React.S.create "" in
  let modal_div =
    R.Html5.div ~a:[a_class ["f-dropdown"; "ck-modal"]; R.Html5.a_style dropdown_style] dropdown in
  let close () =
    ReactiveData.RList.set set_dropdown [];
    set_dropdown_style "" in
  let show ~parent content =
    let left, bottom =
      Js.Opt.case parent
        (fun () -> 10, 10)
        (fun parent ->
            let left, top = pos_from_root parent in
            let height = parent##offsetHeight in
            (left, top + height)) in
    ReactiveData.RList.set set_dropdown content;
    set_dropdown_style (Printf.sprintf "position: absolute; left: %dpx; top: %dpx;" left bottom);
    Ck_modal.show ~close (Tyxml_js.To_dom.of_element modal_div) in
  (show, modal_div)

let current_error, set_current_error = React.S.create None

let make_error_box error =
  error
  |> React.S.map (function
    | None -> pcdata ""
    | Some err ->
        div ~a:[a_class ["ck-bug"; "alert-box"; "alert"]] [
          p [pcdata err]; p [pcdata "Refresh this page to continue."];
        ]
  )
  |> ReactiveData.RList.singleton_s

let () =
  let old_hook = !Lwt.async_exception_hook in
  Lwt.async_exception_hook := (fun ex ->
    old_hook ex;
    let msg = Printexc.to_string ex in
    set_current_error (Some msg)
  )

(* Form submission MUST always return false, or the page will refresh. *)
let a_onsubmit fn =
  a_onsubmit (fun ev ->
    try fn ev; false
    with ex ->
      !Lwt.async_exception_hook ex;
      false
  )

let ck_label s = span ~a:[a_class ["ck-label"]] [pcdata s]

let (>>?=) = Js.Opt.bind

module Make (M : Ck_model_s.MODEL with type gui_data = Gui_tree_data.t) = struct
  module W = M.Widget

  let opt_show ~show_node = function
    | None -> print_endline "Added item no longer exists!"
    | Some node -> show_node (node :> M.Item.generic)

  let class_of_node_type = function
    | `Area _ -> "ck-area"
    | `Dir _ -> "ck-dir"
    | `File _ -> "ck-file"
    | `Deleted -> "ck-deleted"

  let class_of_item item =
    ["ck-item"; class_of_node_type item]

  (* Fade item in and out based on state. *)
  let animated widget child_nodes =
    let li_state =
      W.state widget >|~= fun state ->
        let gui_data = W.gui_data widget in
        let old_item = !gui_data in   (* The original that is being moved, if any *)
        match state, old_item with
        | `Removed _time, _ -> `Fade_out
        | (`New | `Init), Some old_item -> `Fade_in_from old_item
        | `Current, _ -> `No_animation
        | `Init, None -> `No_animation
        | `New, None -> `Fade_in
        in
    let li_cl = li_state |> React.S.map (function
        | `Fade_in -> ["new"]
        | `Fade_out -> ["removed"]
        | `Fade_in_from _ -> ["moved"]
        | `No_animation -> []
    ) in
    let li_elem = li ~a:[R.Html5.a_class li_cl] child_nodes in
    let cancel = ref ignore in
    let animate =
      li_state >|~= fun animation ->
        !cancel ();
        cancel := ignore;
        let gui_data = W.gui_data widget in
        (* Update to point at the new element *)
        gui_data := Some (Tyxml_js.To_dom.of_li li_elem);
        match animation with
        | `Fade_out ->
            let elem = Tyxml_js.To_dom.of_element li_elem in
            cancel := Ck_animate.fade_out elem;
        | `Fade_in_from old_item ->
            let full_height = old_item##offsetHeight in
            cancel := Ck_animate.fade_in_move ~full_height li_elem
        | `Fade_in -> ()    (* Handled by the CSS alone *)
        | `No_animation -> () in
    React.S.retain li_state (fun () -> ignore animate) |> ignore;
    li_elem

  (** Display a floating box for a name, over [button]. If a non-empty name is entered,
   * call [adder name] and display the resulting item. *)
  let show_add_modal ~show_node ~button adder =
    let name_input = input ~a:[a_name "name"; a_placeholder "Name"; a_size 25] () in
    auto_focus name_input;
    let submit_clicked _ev =
      let input_elem = Tyxml_js.To_dom.of_input name_input in
      let name = input_elem##value |> Js.to_string |> String.trim in
      if name <> "" then (
        async ~name:"add" (fun () -> adder name >|= opt_show ~show_node);
      );
      Ck_modal.close () in
    let content =
      form ~a:[a_onsubmit submit_clicked; a_class ["ck-add-modal"]] [
        name_input;
        submit_button "Add";
      ] in
    show_modal ~parent:button [content]

  let report_error ~parent = function
    | `Ok () -> ()
    | `Cancelled_by_user -> ()    (* No need for an alert *)
    | `Error msg ->
        let close _ev = Ck_modal.close (); false in
        let content =
          div ~a:[a_class ["alert-box"]; a_onclick close] [
            pcdata msg;
          ] in
        show_modal ~parent [content]

  let with_adder m ?adder ~show_node item_span =
    match adder with
    | Some adder ->
        let add_clicked ev =
          show_add_modal ~show_node ~button:(ev##target) (M.apply_adder m adder);
          false in
        span [
          item_span;
          a ~a:[a_class ["ck-add-child"]; a_onclick add_clicked] [pcdata "+"]
        ]
    | _ -> span [item_span]

  let render_item m ?adder ~show_node (item : [< M.Item.generic]) =
    let clicked _ev = show_node (item :> M.Item.generic); false in
    let item_cl = class_of_item item in
    span ~a:[a_class item_cl] [
      span ~a:[a_class ["allow-strikethrough"]] [   (* CSS hack to allow strikethrough and underline together *)
        a ~a:[a_class ["ck-title"]; a_onclick clicked] [pcdata (M.Item.name item)];
      ];
    ]
    |> with_adder m ?adder ~show_node

  let render_group_item m ?adder ~show_node item =
    let clicked _ev = show_node (item :> M.Item.generic); false in
    a ~a:[a_onclick clicked; a_class ["ck-group-label"]] [pcdata (M.Item.name item)]
    |> with_adder m ?adder ~show_node

  let group_label m ?adder ~show_node s =
    span ~a:[a_class ["ck-group-label"]] [pcdata s]
    |> with_adder m ?adder ~show_node

  (* A <li>[toggles] name x [children]</li> element *)
  let rec make_tree_node_view m ?(always_full=false) ~show_node widget : _ Html5.elt =
    let item = W.item widget in
    let children = W.children widget
      |> ReactiveData.RList.map (make_tree_node_view m ~always_full ~show_node) in
    let item_html =
      match item with
      | `Item item ->
          ReactiveData.RList.singleton_s item
          |> ReactiveData.RList.map (fun item ->
            let adder = W.adder widget in
            if always_full || W.unique widget then render_item m ~show_node ?adder item
            else render_group_item m ~show_node ?adder item
          )
          |> R.Html5.span
      | `Group label -> group_label m ?adder:(W.adder widget) ~show_node label in
    animated widget [
      item_html;
      R.Html5.ul children;
    ]

  let make_process_view m ~show_node tree =
    let add_clicked ev =
      show_add_modal ~show_node ~button:(ev##target) (fun name ->
        M.add_file m ~name ()
      );
      false in
    [
      h1 [R.Html5.pcdata (M.banner m)];
      h4 [pcdata "Files"; a ~a:[a_class ["ck-add-child"]; a_onclick add_clicked] [pcdata "+"]];
      R.Html5.ul (ReactiveData.RList.map (make_tree_node_view m ~show_node) tree)
    ]

  let make_log_entry m slow_item =
    let item_ref = ref None in
    let cancel = ref ignore in
    let cl =
      Slow_set.state slow_item >|~= fun state ->
        !cancel ();
        match state with
        | `New -> ["new"]
        | `Init | `Current -> []
        | `Removed _ ->
            begin match !item_ref with
            | None -> ()
            | Some item ->
                let elem = Tyxml_js.To_dom.of_li item in
                cancel := Ck_animate.fade_out elem end;
            [] in
    let log_entry = Slow_set.data slow_item in
    let view _ev =
      async ~name:"fix_head" (fun () -> M.fix_head m (Some log_entry));
      false in
    let open Git_storage_s.Log_entry in
    let summary =
      match log_entry.msg with
      | [] -> "(no log message)"
      | x::_ -> x in
    let date = Ck_time.string_of_unix_time log_entry.date in
    let a_cl = M.server_head m >|~= function
      | Some id when log_entry.id = id -> ["ck-server-head"]
      | _ -> [] in
    let item =
      li ~a:[R.Html5.a_class cl] [
        a ~a:[a_onclick view; R.Html5.a_class a_cl] [pcdata (date ^ ": " ^ summary)];
      ] in
    item_ref := Some item;
    item

  let make_sync m =
    let log_elem, log_elem_h = ReactiveData.RList.make [] in
    let log_t = M.enable_log m in
    async ~name:"make_sync" (fun () ->
      log_t >|= fun items ->
      log_elem_h |> ReactiveData.RList.append (
        R.Html5.ol ~a:[a_class ["ck-history"]] (ReactiveData.RList.map (make_log_entry m) items)
      )
    );
    R.Html5.div log_elem

  let make_tree ~show_node m =
    make_process_view m ~show_node (M.tree m)

  let assume_changed _ _ = false

  let add_form ~close ~show_node adder =
    let do_add ev =
      let form = ev##target >>?= Dom_html.CoerceTo.form in
      Js.Opt.iter form (fun form ->
        let f = Form.get_form_contents form in
        let name = List.assoc "name" f |> String.trim in
        if name <> "" then (
          async ~name:"add" (fun () ->
            adder ~name ?description:None () >|= opt_show ~show_node
          )
        );
        close ()
      ) in
    let keydown (ev:Dom_html.keyboardEvent Js.t) =
      if ev##keyCode = keycode_escape then (
        close();
        false
      ) else true in
    let name_input = input ~a:[a_name "name"; a_placeholder "Name"] () in
    auto_focus name_input;
    form ~a:[a_onsubmit do_add; a_onkeydown keydown] [
      name_input;
    ]

  let make_child_adder m ~show_node item =
    let editing, set_editing = React.S.create ~eq:assume_changed None in
    let add_button adder label =
      let start_editing (_:#Dom_html.event Js.t) =
        set_editing (Some adder);
        true in
      li [a ~a:[a_onclick start_editing] [pcdata label]] in
    let widgets =
      editing >>~= (function
        (* When we're not editing, display the add buttons. *)
        | None ->
            item >|~= (function
              | None -> pcdata ""
              | Some item ->
                  match item with
                  | `File _ | `Contact _ | `Context _ -> pcdata ""
                  | `Dir _ as item -> ul ~a:[a_class ["ck-adders"]] [
                      add_button (M.add_file m ~parent:item) "+file";
                    ]
            )
        (* When we are editing, display the form. *)
        | Some adder ->
            let close () = set_editing None in
            React.S.const (add_form ~close ~show_node adder)
      )
    in
    let rlist = ReactiveData.RList.singleton_s widgets in
    R.Html5.div ~a:[a_class ["ck-adders"]] rlist

  let make_editable_title m item =
    let name = item >|~= (function
      | Some item -> M.Item.uuid item |> Ck_id.to_string
      | None -> "(deleted)"
    ) in
    let editing, set_editing = React.S.create None in
    let widgets =
      editing >|~= (function
        | None ->
            let edit _ev =
              begin match React.S.value item with
              | None -> ()
              | Some item -> set_editing (Some item) end;
              true in
            [
              span ~a:[a_class ["allow-strikethrough"]] [   (* CSS hack to allow strikethrough and underline together *)
                a ~a:[a_class ["ck-title"]; a_onclick edit] [R.Html5.pcdata name];
              ]
            ]
        | Some item ->
            let submit ev =
              let form = ev##target >>?= Dom_html.CoerceTo.form in
              Js.Opt.iter form (fun form ->
                let f = Form.get_form_contents form in
                let name = List.assoc "name" f |> String.trim in
                if name <> "" then (
                  async ~name:"set_name" (fun () -> M.set_name m item (Ck_id.of_string name))
                )
              );
              set_editing None in
            let keydown (ev:Dom_html.keyboardEvent Js.t) =
              if ev##keyCode = keycode_escape then (
                set_editing None;
                false
              ) else true in
            let old_name = React.S.value name in
            let name_input =
              input ~a:[a_name "name"; a_placeholder "Name"; a_size 25; a_value old_name; a_onkeydown keydown] () in
            auto_focus name_input;
            [
              form ~a:[a_class ["rename"]; a_onsubmit submit] [
                name_input;
              ]
            ]
      ) in
    rlist_of ~init:(React.S.value widgets) widgets

  let parent_candidates m item =
    M.candidate_parents_for m item |> List.map (fun candidate ->
      let clicked _ev =
        Ck_modal.close ();
        async ~name:"set parent" (fun () -> M.choose_candidate candidate);
        true in
      li [
        a ~a:[a_onclick clicked] [
          pcdata (M.candidate_label candidate)
        ]
      ]
    )

  let make_node_chooser ~edit ~show_node ~if_none current =
    let clicked ev =
      edit ev;
      false in
    match current with
    | None -> [a ~a:[a_onclick clicked] [pcdata if_none]]
    | Some current ->
        let current = (current :> M.Item.generic) in
        let cl = ["ck-item"; class_of_node_type current] in
        let show_clicked _ev = show_node current; false in
        let show_button = a ~a:[a_onclick show_clicked] [pcdata " (show)"] in
        [
          span ~a:[a_class cl] [
            a ~a:[a_class ["ck-title"]; a_onclick clicked] [pcdata (M.Item.name current)]
          ];
          show_button;
        ]

  let make_parent_details m ~show_node details =
    let pair = React.S.l2 ~eq:assume_changed (fun a b -> (a, b)) details.M.details_item details.M.details_parent in
    rlist_of (pair >|~= fun (item, parent) ->
      match item with
      | None -> [pcdata "(deleted)"]
      | Some item ->
      let make_parent item =
        let edit ev =
          let content = ul (parent_candidates m item) in
          show_modal ~parent:(ev##target) [content] in
        make_node_chooser ~edit ~show_node ~if_none:"(no parent)" parent in
      match item with
      | `Dir _ | `File _ as item ->
          match item with
          | `File _ -> ck_label "A file in " :: make_parent item
          | `Dir _ -> ck_label "A dir in " :: make_parent item
    )

  let make_editable_description m item =
    let delete_clicked ev =
      begin match React.S.value item with
      | None -> ()
      | Some item -> async ~name:"delete" (fun () -> M.delete m item >|= report_error ~parent:(ev##target)) end;
      false in
    let editing, set_editing = React.S.create None in
    let elements =
      editing >>~= (function
        | None ->
            item >|~= (function
              | None -> []
              | Some (`File _ as item) ->
                  let edit _ev =
                    let descr = M.Item.description item in
                    set_editing (Some (item, descr));
                    false in
                  let buttons =
                    div ~a:[a_class ["row"]] [
                      div ~a:[a_class ["small-6"; "columns"; "ck-delete"]] [
                        a ~a:[a_onclick delete_clicked] [pcdata "(delete)"];
                      ];
                      div ~a:[a_class ["small-6"; "columns"; "ck-edit"]] [
                        a ~a:[a_onclick edit] [pcdata "(edit)"];
                      ]
                    ] in
                  let content = M.Item.description item in
                  [pre [pcdata content]; buttons]
              | Some (`Dir _) -> []
            );
        | Some (item, descr) ->
            let cancel _ev = set_editing None; false in
            let submit_ref = ref ignore in
            let keydown (ev:Dom_html.keyboardEvent Js.t) =
              if ev##keyCode = 13 && Js.to_bool ev##ctrlKey then (!submit_ref ev; false)
              else true in
            let value = textarea ~a:[a_rows 5; a_onkeydown keydown]
              (pcdata descr) in
            async ~name:"focus" (fun () ->
              let elem = Tyxml_js.To_dom.of_textarea value in
              elem##focus ();
              let len = String.length descr in
              (Obj.magic elem)##setSelectionRange (len, len);
              Lwt.return ()
            );
            let submit _ev =
              let elem = Tyxml_js.To_dom.of_textarea value in
              let v = Js.to_string (elem##value) |> String.trim in
              async ~name:"set_description" (fun () -> M.set_description m item v);
              set_editing None in
            submit_ref := submit;
            React.S.const [
              form ~a:[a_onsubmit submit] [
                value;
                div ~a:[a_class ["actions"]] [
                  a ~a:[a_onclick cancel] [pcdata "(cancel) "];
                  submit_button "OK";
                ]
              ]
            ]
      ) in
    let cl = editing >|~= (function
      | None -> ["description"]
      | Some _ -> ["description-form"]
    ) in
    R.Html5.div ~a:[R.Html5.a_class cl] (rlist_of elements)

  let make_conflicts item =
    item >|~= function
    | None -> []
    | Some item ->
    match M.Item.conflicts item with
    | [] -> []
    | ms ->
        [
          div ~a:[a_class ["ck-conflicts"]] [
            div ~a:[a_class ["ck-box-title"]] [
              h4 [pcdata "Merge conflicts"];
            ];
            ul (
              ms |> List.map (fun msg -> li [pcdata msg])
            )
          ]
        ]

  let make_details_panel m ~set_closed ~show_node details =
    let item = details.M.details_item in
    let title_cl =
      item >|~= (function
        | None ->
            set_closed true;
            ["ck-heading"]
        | Some item ->
            ["ck-heading"; class_of_node_type item]
      ) in
    let children = details.M.details_children
      |> ReactiveData.RList.map (make_tree_node_view m ~show_node) in
    let description = make_editable_description m item in
    let title =
      div ~a:[R.Html5.a_class title_cl] [
        R.Html5.div ~a:[a_class ["inline"]] (make_editable_title m item);
      ] in
    let contents =
      [
        div [
          R.Html5.span (make_parent_details m ~show_node details);
        ];
        R.Html5.div (make_conflicts item |> rlist_of);
        R.Html5.ul ~a:[a_class ["ck-groups"]] children;
        make_child_adder m ~show_node item;
        description;
      ] in
    (title, div contents)

  let history_uuid = Ck_id.of_string "aeeb4ba1-ae68-43ff-b23e-1f66e8b950a3"

  let make_details_area m =
    let details_pane, details_handle = ReactiveData.RList.make [] in
    let remove uuid =
      let current_items = ReactiveData.RList.value details_pane in
      match index_of uuid current_items with
      | None -> ()
      | Some i-> ReactiveData.RList.remove i details_handle in
    let show_or_create uuid make =
      let current_items = ReactiveData.RList.value details_pane in
      let existing =
        try
          Some (List.find (fun (id, _) -> id = uuid) current_items)
        with Not_found -> None in
      match existing with
      | None ->
          let closed, set_closed = React.S.create false in
          let panel = make closed set_closed in
          ReactiveData.RList.insert (uuid, panel) 0 details_handle;
          Ck_animate.scroll_to_show (0, 0)
      | Some _ ->
          Ck_panel.highlight uuid
      in
    let rec show_node (item : [< M.Item.generic]) =
      let uuid = M.Item.uuid item in
      show_or_create uuid (fun closed set_closed ->
        let details = M.details m item in
        let on_destroy () =
          details.M.details_stop ();
          remove uuid in
        let title, contents = make_details_panel m ~set_closed ~show_node details in
        Ck_panel.make ~on_destroy ~closed ~set_closed ~title ~contents ~id:uuid
      ) in
    let show_history () =
      show_or_create history_uuid (fun closed set_closed ->
        let title = b [pcdata "History"] in
        let contents = make_sync m in
        let on_destroy () =
          remove history_uuid;
          M.disable_log m in
        Ck_panel.make ~on_destroy ~closed ~set_closed ~title ~contents ~id:history_uuid
      ) in
    let close_all () =
      ReactiveData.RList.value details_pane
      |> List.iter (fun (_id, panel) -> Ck_panel.close panel) in
    (
      ReactiveData.RList.map (fun (_, panel) -> Ck_panel.element panel) details_pane,
      show_node,
      show_history,
      close_all
    )

  let time_travel_warning m =
    let showing = ref false in
    let return_to_present _ev =
      async ~name:"return_to_present" (fun () -> M.fix_head m None);
      false in
    M.fixed_head m >|~= (function
      | None -> showing := false; []
      | Some log_entry ->
          let cl = ["alert-box"; "ck-time-travel-warning"] in
          let cl = if !showing then cl else "new" :: cl in
          let msg = match log_entry.Git_storage_s.Log_entry.msg with [] -> "-" | summary::_ -> summary in
          let revert ev =
            async ~name:"revert" (fun () -> M.revert m log_entry >|= report_error ~parent:(ev##target));
            false in
          showing := true;
          [
            div ~a:[a_class cl] [
              p [pcdata (
                Printf.sprintf "Time-travel active: you are viewing the state as at %s."
                  (Ck_time.string_of_unix_time log_entry.Git_storage_s.Log_entry.date)
              )];
              p [
                button ~a:[a_onclick revert] [pcdata "Revert this change"];
                pcdata msg;
              ];
              a ~a:[a_class ["close"]; a_onclick return_to_present] [pcdata "×"]
            ]
          ]
    )
    |> rlist_of

  let export m ev =
    let s, set_s = React.S.create (pcdata "Exporting...") in
    async ~name:"export" (fun () ->
      M.export_tar m >|= fun data ->
      let data = make_blob ~mime:"application/x-tar" data in
      let name = "irmin-export.tar" in
      let download _ev =
        save_as data name;
        Ck_modal.close ();
        false in
      set_s (a ~a:[a_onclick download] [pcdata name])
    );
    [R.Html5.div ~a:[a_class ["ck-export"]] (ReactiveData.RList.singleton_s s)]
    |> show_modal ~parent:(ev##target);
    false

  let sync client ev =
    async ~name:"sync" (fun () ->
      M.Client.sync client >|= report_error ~parent:(ev##target)
    );
    false

  let make_top m =
    let details_area, show_node, show_history, close_all = make_details_area m in
    let actions = [
      a ~a:[a_onclick (export m)] [pcdata "Export"];
      a ~a:[a_onclick (fun _ -> show_history (); false)] [pcdata "Show history"];
      a ~a:[a_onclick (fun _ -> close_all (); false)] [pcdata "Close all"];
    ] in
    let actions =
      match M.client m with
      | Some client ->
          let cl = M.Client.sync_in_progress client >|~= (function
            | false -> []
            | true -> ["ck-in-progress"]
          ) in
          a ~a:[a_onclick (sync client); R.Html5.a_class cl] [pcdata "Sync"] :: actions
      | None -> actions in
    let left_panel =
      make_tree ~show_node m in
    [
      modal_div;
      R.Html5.div (make_error_box current_error);
      R.Html5.div (time_travel_warning m);
      div ~a:[a_class ["ck-columns"]] [
        Html5.div ~a:[a_class ["ck-tree"]] (
          left_panel;
        );
        div ~a:[a_class ["ck-details-column"]] [
          div ~a:[a_class ["ck-actions"]] actions;
          R.Html5.div ~a:[a_class ["ck-panels"]] details_area;
        ];
      ];
    ]
end
