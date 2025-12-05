open Graphics
open Types

type button = {
  id : string;
  label : string;
  x : int;
  y : int;
  w : int;
  h : int;
  color : int;
  text_color : int;
}

type notice_kind =
  | Info
  | Success
  | Error

let window_w = 1100
let window_h = 720
let background = rgb 245 247 252
let card = rgb 255 255 255
let border = rgb 225 231 241
let accent = rgb 70 97 245
let accent_dark = rgb 40 55 120
let muted = rgb 110 121 146
let success = rgb 31 164 104
let danger = rgb 200 64 64
let warning = rgb 239 181 80

(** Safely close the graphics window without raising if it's already closed. *)
let safe_close () = try close_graph () with _ -> ()

(** Clear the window to the background color. *)
let clear_canvas () =
  set_color background;
  fill_rect 0 0 window_w window_h;
  set_color black

(** Draw the page title and subtitle at the top of the window. *)
let draw_title ~title ~subtitle =
  set_color accent_dark;
  set_text_size 28;
  moveto 40 (window_h - 70);
  draw_string title;
  set_text_size 16;
  set_color muted;
  moveto 40 (window_h - 100);
  draw_string subtitle

(** Draw a white card with a subtle border. *)
let draw_card ~x ~y ~w ~h =
  set_color card;
  fill_rect x y w h;
  set_color border;
  draw_rect x y w h;
  set_color black

(** Center text inside a rectangle using the provided color. *)
let center_text_in_rect text (x, y, w, h) color =
  let tw, th = text_size text in
  let tx = x + ((w - tw) / 2) in
  let ty = y + ((h - th) / 2) in
  set_color color;
  moveto tx ty;
  draw_string text;
  set_color black

(** Render a clickable button with a label and outline. *)
let draw_button b =
  set_color b.color;
  fill_rect b.x b.y b.w b.h;
  set_color border;
  draw_rect b.x b.y b.w b.h;
  center_text_in_rect b.label (b.x, b.y, b.w, b.h) b.text_color

(** Return the button under a given coordinate if one exists. *)
let button_at buttons x y =
  List.find_opt
    (fun b -> x >= b.x && x <= b.x + b.w && y >= b.y && y <= b.y + b.h)
    buttons

(** Block until a button is chosen via mouse click or mapped keypress, returning
    its id. *)
let wait_for_button buttons =
  let rec loop () =
    let ev = wait_next_event [ Button_down; Key_pressed ] in
    if ev.button then
      match button_at buttons ev.mouse_x ev.mouse_y with
      | Some b -> b.id
      | None -> loop ()
    else if ev.keypressed then
      match ev.key with
      | '\027' -> (
          match List.find_opt (fun b -> b.id = "back") buttons with
          | Some _ -> "back"
          | None -> loop ())
      | '1' .. '9' ->
          let idx = int_of_char ev.key - int_of_char '1' in
          if idx < List.length buttons then (List.nth buttons idx).id
          else loop ()
      | _ -> loop ()
    else loop ()
  in
  loop ()

(** Present a modal notice with a message and an OK button. *)
let show_notice ?(kind = Info) ~title ~body () =
  let card_w = 720 in
  let card_h = 200 in
  let x = (window_w - card_w) / 2 in
  let y = (window_h - card_h) / 2 in
  let bar_color =
    match kind with
    | Info -> accent
    | Success -> success
    | Error -> danger
  in
  draw_card ~x ~y ~w:card_w ~h:card_h;
  set_color bar_color;
  fill_rect x (y + card_h - 8) card_w 8;
  set_color accent_dark;
  set_text_size 20;
  moveto (x + 20) (y + card_h - 45);
  draw_string title;
  set_text_size 14;
  set_color muted;
  moveto (x + 20) (y + card_h - 80);
  draw_string body;
  let ok_button =
    {
      id = "ok";
      label = "OK";
      x = x + (card_w / 2) - 60;
      y = y + 20;
      w = 120;
      h = 40;
      color = bar_color;
      text_color = card;
    }
  in
  draw_button ok_button;
  ignore (wait_for_button [ ok_button ])

(** Render a titled card that lists meetings vertically. *)
let render_meeting_list ~title meetings ~x ~y ~w ~h =
  draw_card ~x ~y ~w ~h;
  set_color accent_dark;
  set_text_size 16;
  moveto (x + 16) (y + h - 32);
  draw_string title;
  set_color muted;
  set_text_size 12;
  let line_y = ref (y + h - 58) in
  if meetings = [] then (
    moveto (x + 16) (y + h - 70);
    draw_string "No meetings yet.")
  else
    List.iter
      (fun m ->
        if !line_y > y + 20 then (
          moveto (x + 16) !line_y;
          draw_string ("- " ^ string_of_meeting m);
          line_y := !line_y - 24))
      meetings

(** Render stacked stat cards at the given origin. *)
let draw_stats ~x ~y stats =
  let w = 320 in
  let h = 120 in
  List.iteri
    (fun idx (label, value, tone) ->
      let block_y = y - (idx * (h + 12)) in
      draw_card ~x ~y:block_y ~w ~h;
      set_color tone;
      fill_rect x block_y 6 h;
      set_color accent_dark;
      set_text_size 16;
      moveto (x + 18) (block_y + h - 36);
      draw_string label;
      set_color muted;
      set_text_size 14;
      moveto (x + 18) (block_y + h - 68);
      draw_string value)
    stats

(** Draw a list of buttons. *)
let render_buttons buttons = List.iter draw_button buttons

(** Prompt for arbitrary text, optionally masking for passwords, with
    submit/cancel handling. *)
let rec prompt_text ?(password = false) ?(allow_empty = false) ~title ~subtitle
    () =
  let rec loop current error_msg =
    clear_canvas ();
    draw_title ~title ~subtitle;
    draw_card ~x:120 ~y:240 ~w:(window_w - 240) ~h:200;
    set_color muted;
    moveto 160 380;
    draw_string "Type your response. Enter = submit, Esc = cancel.";
    let display =
      if password then String.make (String.length current) '*' else current
    in
    set_color accent_dark;
    set_text_size 20;
    moveto 160 320;
    draw_string display;
    (match error_msg with
    | Some msg ->
        set_color danger;
        set_text_size 12;
        moveto 160 280;
        draw_string msg
    | None -> ());
    let ev = wait_next_event [ Key_pressed ] in
    if ev.keypressed then
      match ev.key with
      | '\r' ->
          if allow_empty || String.length current > 0 then Some current
          else loop current (Some "Value cannot be empty.")
      | '\027' -> None
      | '\b' | '\127' ->
          if String.length current = 0 then loop "" error_msg
          else
            let next = String.sub current 0 (String.length current - 1) in
            loop next None
      | c ->
          if Char.code c >= 32 && Char.code c <= 126 then
            let next = current ^ String.make 1 c in
            loop next None
          else loop current error_msg
    else loop current error_msg
  in
  loop "" None

(** Attempt to parse a time string in HH:MM format. *)
let parse_time_input input =
  match String.split_on_char ':' (String.trim input) with
  | [ h; m ] -> (
      try
        let hours = int_of_string h in
        let minutes = int_of_string m in
        if hours >= 0 && hours <= 23 && minutes >= 0 && minutes <= 59 then
          Some { hours; minutes }
        else None
      with _ -> None)
  | _ -> None

(** Attempt to parse a date string in YYYY-MM-DD format. *)
let parse_date_input input =
  match String.split_on_char '-' (String.trim input) with
  | [ y; m; d ] -> (
      try
        let year = int_of_string y in
        let month = int_of_string m in
        let day = int_of_string d in
        if
          year >= 1900 && year <= 2100 && month >= 1 && month <= 12 && day >= 1
          && day <= 31
        then Some { year; month; day }
        else None
      with _ -> None)
  | _ -> None

(** Prompt for a password twice, ensuring non-empty and matching values. *)
let rec prompt_password_pair ~title ~subtitle () =
  match prompt_text ~password:true ~title ~subtitle () with
  | None -> None
  | Some first when String.length first = 0 ->
      show_notice ~kind:Error ~title:"Password required"
        ~body:"Password cannot be empty." ();
      prompt_password_pair ~title ~subtitle ()
  | Some first -> (
      match
        prompt_text ~password:true ~title:"Confirm password"
          ~subtitle:"Re-enter the same password" ()
      with
      | Some second when second = first -> Some first
      | Some _ ->
          show_notice ~kind:Error ~title:"Passwords do not match"
            ~body:"Please try again." ();
          prompt_password_pair ~title ~subtitle ()
      | None -> None)

(** Ensure the host account exists, prompting creation if missing. *)
let rec ensure_host_account () =
  if Auth.host_exists () then true
  else
    match
      prompt_password_pair ~title:"Create host password"
        ~subtitle:"This secures the host dashboard." ()
    with
    | None -> false
    | Some password -> (
        match Auth.create_user "host" password with
        | Ok _ ->
            show_notice ~kind:Success ~title:"Host ready"
              ~body:"Host account created successfully." ();
            true
        | Error msg ->
            show_notice ~kind:Error ~title:"Unable to create host" ~body:msg ();
            ensure_host_account ())

(** Prompt for a password for [username], looping on failure until success or
    cancel. *)
let rec authenticate username =
  match
    prompt_text ~password:true
      ~title:("Authenticate " ^ username)
      ~subtitle:"Enter your password to continue." ()
  with
  | None -> None
  | Some password ->
      if Auth.verify_user_password username password then Some ()
      else (
        show_notice ~kind:Error ~title:"Authentication failed"
          ~body:"Incorrect password. Try again." ();
        authenticate username)

(** Let a user review and respond to their pending invitations. *)
let rec review_invitations username =
  let invitations = Storage.get_invitations_for_user username in
  match invitations with
  | [] ->
      show_notice ~kind:Info ~title:"No pending invitations"
        ~body:"You're all caught up." ()
  | _ ->
      let total = List.length invitations in
      let rec loop idx =
        if idx >= total then ()
        else
          let inv = List.nth invitations idx in
          clear_canvas ();
          draw_title
            ~title:("Invitations for " ^ username)
            ~subtitle:(Printf.sprintf "Reviewing %d of %d" (idx + 1) total);
          draw_card ~x:120 ~y:200 ~w:(window_w - 240) ~h:280;
          set_color accent_dark;
          set_text_size 18;
          moveto 160 430;
          draw_string (string_of_meeting inv);
          set_color muted;
          set_text_size 14;
          moveto 160 390;
          draw_string
            "Options: Accept to schedule, Decline to remove, Skip to keep.";
          let buttons =
            [
              {
                id = "accept";
                label = "Accept";
                x = 160;
                y = 240;
                w = 160;
                h = 50;
                color = success;
                text_color = card;
              };
              {
                id = "decline";
                label = "Decline";
                x = 360;
                y = 240;
                w = 160;
                h = 50;
                color = danger;
                text_color = card;
              };
              {
                id = "skip";
                label = "Skip";
                x = 560;
                y = 240;
                w = 160;
                h = 50;
                color = warning;
                text_color = accent_dark;
              };
              {
                id = "back";
                label = "Back to dashboard";
                x = 760;
                y = 240;
                w = 180;
                h = 50;
                color = accent;
                text_color = card;
              };
            ]
          in
          render_buttons buttons;
          match wait_for_button buttons with
          | "accept" -> (
              match Scheduler.schedule_meeting inv with
              | Ok _ ->
                  Storage.remove_invitation (fun cand -> cand = inv);
                  show_notice ~kind:Success ~title:"Invitation accepted"
                    ~body:"Meeting scheduled successfully." ();
                  review_invitations username
              | Error msg ->
                  show_notice ~kind:Error ~title:"Unable to schedule" ~body:msg
                    ();
                  loop (idx + 1))
          | "decline" ->
              Storage.remove_invitation (fun cand -> cand = inv);
              show_notice ~kind:Info ~title:"Invitation declined"
                ~body:"The invitation was removed." ();
              review_invitations username
          | "skip" -> loop (idx + 1)
          | "back" -> ()
          | _ -> loop idx
      in
      loop 0

(** Guide a user through entering invitation details and enqueue the invite if
    valid. *)
let rec schedule_meeting_gui organizer_name =
  match
    prompt_text ~title:"Send an invitation"
      ~subtitle:"Who would you like to meet with?" ()
  with
  | None -> ()
  | Some invitee when String.trim invitee = "" ->
      show_notice ~kind:Error ~title:"Invitee required"
        ~body:"Invitee name cannot be empty." ();
      schedule_meeting_gui organizer_name
  | Some invitee when invitee = organizer_name ->
      show_notice ~kind:Error ~title:"Cannot invite yourself"
        ~body:"Choose someone else to invite." ();
      schedule_meeting_gui organizer_name
  | Some invitee -> (
      match
        ( prompt_text ~title:"Date" ~subtitle:"Enter date (YYYY-MM-DD)" (),
          prompt_text ~title:"Start time" ~subtitle:"Enter start time (HH:MM)"
            (),
          prompt_text ~title:"End time" ~subtitle:"Enter end time (HH:MM)" () )
      with
      | None, _, _ | _, None, _ | _, _, None -> ()
      | Some date_str, Some start_str, Some end_str -> (
          match
            ( parse_date_input date_str,
              parse_time_input start_str,
              parse_time_input end_str )
          with
          | Some date, Some start_time, Some end_time ->
              if not (is_valid_interval start_time end_time) then
                show_notice ~kind:Error ~title:"Invalid time range"
                  ~body:"Start time must be before end time." ()
              else
                let meeting =
                  {
                    organizer_name;
                    attendee_name = invitee;
                    date;
                    start_time;
                    end_time;
                  }
                in
                let existing = Storage.get_all_meetings () in
                if Scheduler.has_conflict meeting existing then
                  show_notice ~kind:Error ~title:"Conflicting meeting"
                    ~body:
                      "A meeting already exists for one of the participants in \
                       that slot."
                    ()
                else (
                  Storage.add_invitation meeting;
                  show_notice ~kind:Success ~title:"Invitation sent"
                    ~body:
                      "The invitee will see this request the next time they \
                       log in."
                    ();
                  ())
          | _ ->
              show_notice ~kind:Error ~title:"Invalid date/time"
                ~body:"Please use YYYY-MM-DD and HH:MM formats." ();
              schedule_meeting_gui organizer_name))

(** Host dashboard for meetings overview, pending invites, and navigation. *)
let rec host_dashboard () =
  let meetings = Storage.get_all_meetings () in
  let pending = Storage.get_invitations_for_user "host" in
  clear_canvas ();
  draw_title ~title:"Host dashboard"
    ~subtitle:"Schedule invitations, review requests, and monitor meetings.";
  render_meeting_list ~title:"All meetings" meetings ~x:40 ~y:140 ~w:660 ~h:480;
  draw_stats ~x:740 ~y:580
    [
      ("Meetings scheduled", string_of_int (List.length meetings), accent);
      ("Pending invites", string_of_int (List.length pending), warning);
    ];
  let buttons =
    [
      {
        id = "schedule";
        label = "Send invitation";
        x = 740;
        y = 360;
        w = 320;
        h = 60;
        color = accent;
        text_color = card;
      };
      {
        id = "pending";
        label = Printf.sprintf "Review invitations (%d)" (List.length pending);
        x = 740;
        y = 290;
        w = 320;
        h = 60;
        color = warning;
        text_color = accent_dark;
      };
      {
        id = "refresh";
        label = "Refresh";
        x = 740;
        y = 220;
        w = 150;
        h = 50;
        color = accent_dark;
        text_color = card;
      };
      {
        id = "logout";
        label = "Logout";
        x = 910;
        y = 220;
        w = 150;
        h = 50;
        color = danger;
        text_color = card;
      };
    ]
  in
  render_buttons buttons;
  match wait_for_button buttons with
  | "schedule" ->
      schedule_meeting_gui "host";
      host_dashboard ()
  | "pending" ->
      review_invitations "host";
      host_dashboard ()
  | "refresh" -> host_dashboard ()
  | "logout" -> ()
  | _ -> host_dashboard ()

(** Attendee dashboard showing personal meetings and invitation actions. *)
let rec attendee_dashboard username =
  let meetings = Storage.get_meetings_for_user username in
  let pending = Storage.get_invitations_for_user username in
  clear_canvas ();
  draw_title
    ~title:("Attendee dashboard - " ^ username)
    ~subtitle:"View meetings, send invitations, and respond to requests.";
  render_meeting_list ~title:"Your meetings" meetings ~x:40 ~y:140 ~w:660 ~h:480;
  draw_stats ~x:740 ~y:580
    [
      ("Meetings for you", string_of_int (List.length meetings), accent);
      ("Pending invites", string_of_int (List.length pending), warning);
    ];
  let buttons =
    [
      {
        id = "schedule";
        label = "Send invitation";
        x = 740;
        y = 360;
        w = 320;
        h = 60;
        color = accent;
        text_color = card;
      };
      {
        id = "pending";
        label = Printf.sprintf "Review invitations (%d)" (List.length pending);
        x = 740;
        y = 290;
        w = 320;
        h = 60;
        color = warning;
        text_color = accent_dark;
      };
      {
        id = "refresh";
        label = "Refresh";
        x = 740;
        y = 220;
        w = 150;
        h = 50;
        color = accent_dark;
        text_color = card;
      };
      {
        id = "logout";
        label = "Logout";
        x = 910;
        y = 220;
        w = 150;
        h = 50;
        color = danger;
        text_color = card;
      };
    ]
  in
  render_buttons buttons;
  match wait_for_button buttons with
  | "schedule" ->
      schedule_meeting_gui username;
      attendee_dashboard username
  | "pending" ->
      review_invitations username;
      attendee_dashboard username
  | "refresh" -> attendee_dashboard username
  | "logout" -> ()
  | _ -> attendee_dashboard username

(** Enter host mode by ensuring the host account, authenticating, then opening
    the dashboard. *)
let rec enter_host_mode () =
  if not (ensure_host_account ()) then ()
  else
    match authenticate "host" with
    | None -> ()
    | Some _ ->
        review_invitations "host";
        host_dashboard ()

(** Enter attendee mode by authenticating existing users or creating a new
    account. *)
let rec enter_attendee_mode () =
  match
    prompt_text ~title:"Attendee login"
      ~subtitle:"Enter your username to continue." ()
  with
  | None -> ()
  | Some username -> (
      let username = String.trim username in
      if String.length username = 0 then (
        show_notice ~kind:Error ~title:"Username required"
          ~body:"Please enter a username." ();
        enter_attendee_mode ())
      else if Auth.user_exists username then (
        match authenticate username with
        | None -> ()
        | Some _ ->
            review_invitations username;
            attendee_dashboard username)
      else
        match
          prompt_password_pair ~title:"Create account"
            ~subtitle:"Set a password for your new account." ()
        with
        | None -> ()
        | Some password -> (
            match Auth.create_user username password with
            | Ok _ ->
                show_notice ~kind:Success ~title:"Account created"
                  ~body:"Welcome aboard! You're now signed in." ();
                attendee_dashboard username
            | Error msg ->
                show_notice ~kind:Error ~title:"Unable to create account"
                  ~body:msg ();
                enter_attendee_mode ()))

(** Landing menu for choosing host mode, attendee mode, or exiting the app. *)
let rec main_menu () =
  clear_canvas ();
  draw_title ~title:"Git Pushin Calendar"
    ~subtitle:"Schedule meetings, review invites, and keep everyone aligned.";
  draw_card ~x:120 ~y:180 ~w:(window_w - 240) ~h:360;
  set_color accent_dark;
  set_text_size 22;
  moveto 200 470;
  draw_string "Choose how you'd like to continue:";
  let buttons =
    [
      {
        id = "host";
        label = "Host mode";
        x = 200;
        y = 380;
        w = 720;
        h = 60;
        color = accent;
        text_color = card;
      };
      {
        id = "attendee";
        label = "Attendee mode";
        x = 200;
        y = 300;
        w = 720;
        h = 60;
        color = accent_dark;
        text_color = card;
      };
      {
        id = "exit";
        label = "Exit";
        x = 200;
        y = 220;
        w = 720;
        h = 60;
        color = danger;
        text_color = card;
      };
    ]
  in
  render_buttons buttons;
  match wait_for_button buttons with
  | "host" ->
      enter_host_mode ();
      main_menu ()
  | "attendee" ->
      enter_attendee_mode ();
      main_menu ()
  | "exit" -> safe_close ()
  | _ -> main_menu ()

(** Open the graphics window, run the main menu loop, and close safely on exit
    or error. *)
let run () =
  try
    open_graph (Printf.sprintf " %dx%d" window_w window_h);
    set_window_title "Git Pushin Calendar";
    auto_synchronize true;
    main_menu ();
    safe_close ()
  with e ->
    safe_close ();
    raise e
