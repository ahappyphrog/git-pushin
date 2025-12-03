open Bogue
open Types

module W = Widget
module L = Layout
module M = Main
module Sy = Sync
module AU = Auth

type session =
  | Host_session
  | Attendee_session of string

type page =
  | Landing
  | Attendee_name
  | Login_host
  | Login_attendee of string
  | Dashboard of session

let red_halo =
  Style.color_bg (Draw.set_alpha 40 Draw.red)
  |> Style.of_bg
  |> L.style_bg

let mark_error layout = L.set_background layout (Some red_halo)
let clear_error layout = L.set_background layout None

let current_username = function
  | Host_session -> "host"
  | Attendee_session name -> name

let compare_meeting m1 m2 =
  match compare_date m1.date m2.date with
  | 0 -> compare_time m1.start_time m2.start_time
  | x -> x

let format_meetings session =
  let meetings =
    match session with
    | Host_session -> Storage.get_all_meetings ()
    | Attendee_session name -> Storage.get_meetings_for_user name
  in
  match List.sort compare_meeting meetings with
  | [] -> "No meetings scheduled yet."
  | ms -> String.concat "\n" (List.map string_of_meeting ms)

let make_header ~title ~on_back ~on_exit ?(actions = []) () =
  let back = W.icon ~size:20 "arrow-left" in
  W.on_click back ~click:(fun _ -> on_back ());
  let exit = W.icon ~size:18 ~fg:(Draw.opaque Draw.red) "times" in
  W.on_click exit ~click:(fun _ -> on_exit ());
  let left = L.flat_of_w ~sep:10 [ back; W.label title ] in
  let right = L.flat_of_w ~sep:10 (actions @ [ exit ]) in
  L.flat ~sep:20 [ left; right ]

let parse_date text =
  match String.split_on_char '-' (String.trim text) with
  | [ y; m; d ] -> (
      try
        let year = int_of_string y in
        let month = int_of_string m in
        let day = int_of_string d in
        if year >= 1900 && month >= 1 && month <= 12 && day >= 1 && day <= 31
        then Some { year; month; day }
        else None
      with _ -> None)
  | _ -> None

let parse_time text =
  match String.split_on_char ':' (String.trim text) with
  | [ h; m ] -> (
      try
        let hours = int_of_string h in
        let minutes = int_of_string m in
        if hours >= 0 && hours <= 23 && minutes >= 0 && minutes <= 59 then
          Some { hours; minutes }
        else None
      with _ -> None)
  | _ -> None

let schedule_section session refresh_all =
  let organizer = current_username session in
  let attendee_input = W.text_input ~prompt:"With (username)" () in
  let attendee_row =
    L.flat_of_w ~sep:10 [ W.label "Attendee"; attendee_input ]
  in

  let date_input = W.text_input ~prompt:"YYYY-MM-DD" () in
  let date_row = L.flat_of_w ~sep:10 [ W.label "Date"; date_input ] in

  let start_input = W.text_input ~prompt:"Start HH:MM" () in
  let start_row =
    L.flat_of_w ~sep:10 [ W.label "Start time"; start_input ]
  in

  let end_input = W.text_input ~prompt:"End HH:MM" () in
  let end_row =
    L.flat_of_w ~sep:10 [ W.label "End time"; end_input ]
  in

  let status = W.label "" in

  let clear_all_errors () =
    List.iter clear_error [ attendee_row; date_row; start_row; end_row ]
  in

  let submit = W.button "Send invitation" in
  W.on_click submit ~click:(fun _ ->
      Sy.push (fun () ->
          clear_all_errors ();
          let attendee = String.trim (W.get_text attendee_input) in
          let date_text = W.get_text date_input in
          let start_text = W.get_text start_input in
          let end_text = W.get_text end_input in

          let ok = ref true in

          if attendee = "" || attendee = organizer then (
            mark_error attendee_row;
            ok := false);

          let date =
            match parse_date date_text with
            | Some d -> d
            | None ->
                mark_error date_row;
                ok := false;
                { year = 0; month = 0; day = 0 }
          in
          let start_time =
            match parse_time start_text with
            | Some t -> t
            | None ->
                mark_error start_row;
                ok := false;
                { hours = 0; minutes = 0 }
          in
          let end_time =
            match parse_time end_text with
            | Some t -> t
            | None ->
                mark_error end_row;
                ok := false;
                { hours = 0; minutes = 0 }
          in

          if !ok && not (is_valid_interval start_time end_time) then (
            mark_error start_row;
            mark_error end_row;
            ok := false);

          if !ok then (
            W.set_text status "Please correct the highlighted fields.")
          else
            let meeting =
              { organizer_name = organizer; attendee_name = attendee; date; start_time; end_time }
            in
            Storage.add_invitation meeting;
            W.set_text status "Invitation sent.";
            W.set_text attendee_input "";
            W.set_text date_input "";
            W.set_text start_input "";
            W.set_text end_input "";
            refresh_all ()));

  L.tower ~sep:12
    [
      W.label "Schedule a meeting" |> L.resident;
      attendee_row;
      date_row;
      start_row;
      end_row;
      submit |> L.resident;
      status |> L.resident;
    ]

let invitations_section username refresh_all =
  let container = L.tower [] in
  let status = W.label "" in

  let rec render () =
    let invitations = Storage.get_invitations_for_user username in
    let rows =
      if invitations = [] then
        [ W.label "No pending invitations." |> L.resident ]
      else
        List.map
          (fun inv ->
            let text = W.label (string_of_meeting inv) in
            let accept = W.button "Accept" in
            let decline = W.button "Decline" in
            W.on_click accept ~click:(fun _ ->
                Sy.push (fun () ->
                    match Scheduler.schedule_meeting inv with
                    | Ok _ ->
                        Storage.remove_invitation (fun candidate -> candidate = inv);
                        W.set_text status "Invitation accepted.";
                        render ();
                        refresh_all ()
                    | Error msg ->
                        W.set_text status ("Unable to schedule: " ^ msg);
                        refresh_all ()));
            W.on_click decline ~click:(fun _ ->
                Sy.push (fun () ->
                    Storage.remove_invitation (fun candidate -> candidate = inv);
                    W.set_text status "Invitation declined.";
                    render ();
                    refresh_all ()));
            L.flat_of_w ~sep:10 [ text; accept; decline ])
          invitations
    in
    L.set_rooms container rows
  in

  render ();
  ( L.tower ~sep:8
      [
        W.label "Pending invitations" |> L.resident;
        container;
        status |> L.resident;
      ],
    render )

let dashboard_page session ~on_back ~on_exit =
  let meetings_view =
    W.text_display ~w:720 ~h:200 (format_meetings session)
  in
  let refresh_icon = W.icon ~size:18 "refresh" in

  let refresh_meetings () =
    W.set_text meetings_view (format_meetings session)
  in

  let header =
    make_header ~title:"Calendar" ~on_back ~on_exit ~actions:[ refresh_icon ] ()
  in

  let refresh_invites = ref (fun () -> ()) in
  let refresh_all () =
    refresh_meetings ();
    (!refresh_invites) ()
  in
  let invitations_layout, refresh_invites_fn =
    invitations_section (current_username session) refresh_all
  in
  refresh_invites := refresh_invites_fn;

  W.on_click refresh_icon ~click:(fun _ -> refresh_all ());

  let meetings_section =
    L.tower ~sep:8
      [
        W.label "Current meetings" |> L.resident;
        meetings_view |> L.resident;
      ]
  in

  L.tower ~sep:20 ~margins:20
    [
      header;
      meetings_section;
      invitations_layout;
      schedule_section session refresh_all;
    ]

let host_login_page ~go_dashboard ~on_back ~on_exit =
  let password_input = W.text_input ~prompt:"Password" () in
  let password_row =
    L.flat_of_w ~sep:10 [ W.label "Password"; password_input ]
  in
  let status = W.label "" in
  let submit = W.button "Submit" in

  W.on_click submit ~click:(fun _ ->
      clear_error password_row;
      let password = String.trim (W.get_text password_input) in
      if password = "" then (
        mark_error password_row;
        W.set_text status "Password required.")
      else if AU.host_exists () then
        if AU.verify_user_password "host" password then
          go_dashboard Host_session
        else W.set_text status "Incorrect password."
      else
        match AU.create_user "host" password with
        | Ok _ ->
            W.set_text status "Host account created.";
            go_dashboard Host_session
        | Error msg -> W.set_text status msg);

  let header =
    make_header ~title:"Host login" ~on_back ~on_exit ()
  in

  L.tower ~sep:15 ~margins:20
    [
      header;
      L.tower ~sep:10
        [
          W.label "Enter your password" |> L.resident;
          password_row;
          submit |> L.resident;
          status |> L.resident;
        ];
    ]

let attendee_login_page name ~go_dashboard ~on_back ~on_exit =
  let password_input = W.text_input ~prompt:"Password" () in
  let password_row =
    L.flat_of_w ~sep:10 [ W.label "Password"; password_input ]
  in
  let status = W.label "" in
  let submit = W.button "Submit" in

  W.on_click submit ~click:(fun _ ->
      clear_error password_row;
      let password = String.trim (W.get_text password_input) in
      if password = "" then (
        mark_error password_row;
        W.set_text status "Password required.")
      else if AU.user_exists name then
        if AU.verify_user_password name password then
          go_dashboard (Attendee_session name)
        else W.set_text status "Incorrect password."
      else
        match AU.create_user name password with
        | Ok _ ->
            W.set_text status "Account created.";
            go_dashboard (Attendee_session name)
        | Error msg -> W.set_text status msg);

  let header =
    make_header ~title:("Attendee login (" ^ name ^ ")") ~on_back ~on_exit ()
  in

  L.tower ~sep:15 ~margins:20
    [
      header;
      L.tower ~sep:10
        [
          W.label "Enter your password" |> L.resident;
          password_row;
          submit |> L.resident;
          status |> L.resident;
        ];
    ]

let attendee_name_page ~go_next ~on_back ~on_exit =
  let name_input = W.text_input ~prompt:"Your name" () in
  let name_row =
    L.flat_of_w ~sep:10 [ W.label "Name"; name_input ]
  in
  let status = W.label "" in
  let submit = W.button "Continue" in

  W.on_click submit ~click:(fun _ ->
      clear_error name_row;
      let name = String.trim (W.get_text name_input) in
      if name = "" then (
        mark_error name_row;
        W.set_text status "Name is required.")
      else go_next name);

  let header =
    make_header ~title:"Attendee" ~on_back ~on_exit ()
  in
  L.tower ~sep:15 ~margins:20
    [
      header;
      L.tower ~sep:10
        [
          W.label "Enter your name to continue" |> L.resident;
          name_row;
          submit |> L.resident;
          status |> L.resident;
        ];
    ]

let landing_page ~on_back ~on_exit =
  let host_btn = W.button "Host" in
  let attendee_btn = W.button "Attendee" in
  let header =
    make_header ~title:"Calendar" ~on_back ~on_exit ()
  in
  L.tower ~sep:20 ~margins:20
    [
      header;
      W.label "Welcome! Choose your role:" |> L.resident;
      L.tower_of_w ~sep:12 [ host_btn; attendee_btn ];
    ],
  host_btn,
  attendee_btn

let run () =
  let placeholder = W.label "" |> L.resident in
  let content = L.tower [ placeholder ] in
  let history = ref [ Landing ] in

  let rec render ?(push = true) page =
    if push then history := page :: !history;
    let layout = make_layout page in
    L.set_rooms content [ layout ]

  and go_to page = render page

  and go_back () =
    match !history with
    | _current :: prev :: rest ->
        history := prev :: rest;
        render ~push:false prev
    | _ -> render ~push:false Landing

  and make_layout = function
    | Landing ->
        let layout, host_btn, attendee_btn =
          landing_page ~on_back:go_back ~on_exit:(fun () -> raise M.Exit)
        in
        W.on_click host_btn ~click:(fun _ -> go_to Login_host);
        W.on_click attendee_btn ~click:(fun _ -> go_to Attendee_name);
        layout
    | Attendee_name ->
        attendee_name_page
          ~go_next:(fun name -> go_to (Login_attendee name))
          ~on_back:go_back ~on_exit:(fun () -> raise M.Exit)
    | Login_host ->
        host_login_page
          ~go_dashboard:(fun session -> go_to (Dashboard session))
          ~on_back:go_back ~on_exit:(fun () -> raise M.Exit)
    | Login_attendee name ->
        attendee_login_page name
          ~go_dashboard:(fun session -> go_to (Dashboard session))
          ~on_back:go_back ~on_exit:(fun () -> raise M.Exit)
    | Dashboard session ->
        dashboard_page session ~on_back:go_back ~on_exit:(fun () -> raise M.Exit)
  in

  render ~push:false Landing;
  M.run (M.of_layout content);
  M.quit ()
