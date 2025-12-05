open Types

(* Read password *)
let read_password prompt =
  Printf.printf "%s: " prompt;
  flush stdout;
  let password = read_line () in
  password

(* Setup host password on first run *)
let setup_host_password () =
  if not (Auth.host_exists ()) then (
    print_endline "\n========================================";
    print_endline "    First Time Setup - Host Password";
    print_endline "========================================";
    print_endline "Please create a password for the host account.";
    print_endline "You will need this password to view all meetings.\n";
    let rec get_password () =
      let pass1 = read_password "Enter password" in
      let pass2 = read_password "Confirm password" in
      if pass1 = pass2 && String.length pass1 > 0 then pass1
      else (
        print_endline "Passwords don't match or are empty. Please try again.\n";
        get_password ())
    in
    let password = get_password () in
    match Auth.create_user "host" password with
    | Ok _ -> print_endline "\nHost password created successfully!"
    | Error msg -> Printf.printf "\nError: %s\n" msg)

(* Authenticate user (create account if doesn't exist) *)
let authenticate_user username =
  if Auth.user_exists username then
    let password = read_password "Enter your password" in
    if Auth.verify_user_password username password then (
      print_endline "Authentication successful!";
      true)
    else (
      print_endline "Incorrect password!";
      false)
  else (
    Printf.printf "\nUser '%s' not found. Let's create an account.\n" username;
    let rec get_password () =
      let pass1 = read_password "Create a password" in
      let pass2 = read_password "Confirm password" in
      if pass1 = pass2 && String.length pass1 > 0 then pass1
      else (
        print_endline "Passwords don't match or are empty. Please try again.\n";
        get_password ())
    in
    let password = get_password () in
    match Auth.create_user username password with
    | Ok msg ->
        print_endline msg;
        true
    | Error msg ->
        Printf.printf "Error: %s\n" msg;
        false)

(* Read and parse a time from user input *)
let rec read_time prompt =
  Printf.printf "%s (HH:MM): " prompt;
  flush stdout;
  try
    let input = read_line () in
    match String.split_on_char ':' input with
    | [ h; m ] ->
        let hours = int_of_string (String.trim h) in
        let minutes = int_of_string (String.trim m) in
        if hours >= 0 && hours <= 23 && minutes >= 0 && minutes <= 59 then
          { hours; minutes }
        else (
          print_endline "Invalid time. Hours must be 0-23, minutes 0-59.";
          read_time prompt)
    | _ ->
        print_endline "Invalid format. Please use HH:MM.";
        read_time prompt
  with _ ->
    print_endline "Invalid input. Please try again.";
    read_time prompt

(* Read and parse a date from user input *)
let rec read_date prompt =
  Printf.printf "%s (YYYY-MM-DD): " prompt;
  flush stdout;
  try
    let input = read_line () in
    match String.split_on_char '-' input with
    | [ y; m; d ] ->
        let year = int_of_string (String.trim y) in
        let month = int_of_string (String.trim m) in
        let day = int_of_string (String.trim d) in
        if
          year >= 1900 && year <= 2100 && month >= 1 && month <= 12 && day >= 1
          && day <= 31
        then { year; month; day }
        else (
          print_endline "Invalid date. Please check your input.";
          read_date prompt)
    | _ ->
        print_endline "Invalid format. Please use YYYY-MM-DD.";
        read_date prompt
  with _ ->
    print_endline "Invalid input. Please try again.";
    read_date prompt

let rec prompt_invitee current_user =
  print_string
    "Enter the username you would like to meet with (or leave blank to \
     cancel): ";
  flush stdout;
  let invitee = read_line () |> String.trim in
  if String.length invitee = 0 then None
  else if invitee = current_user then (
    print_endline "You cannot schedule a meeting with yourself.";
    prompt_invitee current_user)
  else Some invitee

let rec schedule_meeting_for name =
  print_endline "\n--- Schedule a Meeting ---";
  match prompt_invitee name with
  | None -> print_endline "Invite cancelled."
  | Some invitee ->
      let date = read_date "Enter date" in
      let start_time = read_time "Enter start time" in
      let end_time = read_time "Enter end time" in

      let meeting =
        {
          organizer_name = name;
          attendee_name = invitee;
          date;
          start_time;
          end_time;
        }
      in
      if not (is_valid_interval start_time end_time) then
        print_endline
          "Invalid time interval. Start time must be before end time. \
           Invitation not sent."
      else
        let existing_meetings = Storage.get_all_meetings () in
        if Scheduler.has_conflict meeting existing_meetings then
          print_endline
            "A conflicting meeting already exists for you or the invitee. \
             Invitation not sent."
        else (
          Storage.add_invitation meeting;
          Printf.printf
            "\nInvitation sent to %s. They will be prompted to accept.\n"
            invitee;
          if not (Auth.user_exists invitee) then
            print_endline
              "The invitee is new to the system and will be asked to accept \
               upon account creation.";
          ())

let show_attendee_meetings name =
  let meetings = Storage.get_meetings_for_user name in
  if List.length meetings = 0 then
    Printf.printf "\nNo meetings found for %s.\n" name
  else (
    Printf.printf "\nMeetings for %s:\n" name;
    List.iter
      (fun m ->
        let counterpart =
          if m.organizer_name = name then m.attendee_name else m.organizer_name
        in
        Printf.printf "  - With %s: %s\n" counterpart (string_of_meeting m))
      meetings)

let show_all_meetings () =
  let meetings = Storage.get_all_meetings () in
  if List.length meetings = 0 then print_endline "\nNo meetings scheduled yet."
  else (
    Printf.printf "\nTotal meetings: %d\n" (List.length meetings);
    List.iter (fun m -> print_endline ("  - " ^ string_of_meeting m)) meetings)

let rec handle_pending_invitations username =
  let invitations = Storage.get_invitations_for_user username in
  match invitations with
  | [] -> print_endline "\nNo pending invitations."
  | _ ->
      print_endline "\nPending invitations:";
      List.iter
        (fun inv ->
          Printf.printf "\nFrom %s: %s\n" inv.organizer_name
            (string_of_meeting inv);
          let rec prompt_response () =
            print_string "Accept this invitation? (y/n/skip): ";
            flush stdout;
            match read_line () with
            | "y" | "Y" -> begin
                match Scheduler.schedule_meeting inv with
                | Ok _ ->
                    Storage.remove_invitation (fun candidate -> candidate = inv);
                    print_endline "Invitation accepted and meeting scheduled."
                | Error msg ->
                    Printf.printf "Unable to schedule meeting: %s\n" msg
              end
            | "n" | "N" ->
                Storage.remove_invitation (fun candidate -> candidate = inv);
                print_endline "Invitation declined."
            | "s" | "S" -> print_endline "Invitation skipped for now."
            | _ ->
                print_endline "Please respond with 'y', 'n', or 's'.";
                prompt_response ()
          in
          prompt_response ())
        invitations

let rec host_dashboard () =
  print_endline "\n========================================";
  print_endline "           Host Dashboard";
  print_endline "========================================";
  show_all_meetings ();
  print_endline "\nOptions:";
  print_endline "1. Refresh meetings";
  print_endline "2. Send meeting invitation";
  print_endline "3. Review pending invitations";
  print_endline "4. Logout";
  print_string "Select an option (1-4): ";
  flush stdout;
  match read_line () with
  | "1" -> host_dashboard ()
  | "2" ->
      schedule_meeting_for "host";
      host_dashboard ()
  | "3" ->
      handle_pending_invitations "host";
      host_dashboard ()
  | "4" -> print_endline "Logging out of host mode..."
  | _ ->
      print_endline "Invalid option. Please try again.";
      host_dashboard ()

let rec attendee_dashboard name =
  print_endline "\n========================================";
  Printf.printf "       Attendee Dashboard - %s\n" name;
  print_endline "========================================";
  show_attendee_meetings name;
  print_endline "\nOptions:";
  print_endline "1. Send meeting invitation";
  print_endline "2. Refresh meetings";
  print_endline "3. Review pending invitations";
  print_endline "4. Logout";
  print_string "Select an option (1-4): ";
  flush stdout;
  match read_line () with
  | "1" ->
      schedule_meeting_for name;
      attendee_dashboard name
  | "2" -> attendee_dashboard name
  | "3" ->
      handle_pending_invitations name;
      attendee_dashboard name
  | "4" -> print_endline "Logging out of attendee mode..."
  | _ ->
      print_endline "Invalid option. Please try again.";
      attendee_dashboard name

let rec enter_host_mode () =
  print_endline "\n--- Host Mode ---";
  setup_host_password ();
  print_endline "Please authenticate to access host features.";
  if authenticate_user "host" then (
    print_endline "Authentication successful!";
    handle_pending_invitations "host";
    host_dashboard ())
  else print_endline "Authentication failed. Returning to mode selection."

let rec enter_attendee_mode () =
  print_endline "\n--- Attendee Mode ---";
  print_string "Enter your username: ";
  flush stdout;
  let name = read_line () |> String.trim in
  if String.length name = 0 then (
    print_endline "Username cannot be empty.";
    enter_attendee_mode ())
  else if authenticate_user name then (
    print_endline "Authentication successful!";
    handle_pending_invitations name;
    attendee_dashboard name)
  else print_endline "Authentication failed. Returning to mode selection."

let rec mode_selection_menu () =
  print_endline "\n========================================";
  print_endline "        Calendar Application";
  print_endline "========================================";
  print_endline "1. Host mode";
  print_endline "2. Attendee mode";
  print_endline "3. Exit";
  print_endline "========================================";
  print_string "Select an option (1-3): ";
  flush stdout;
  match read_line () with
  | "1" ->
      enter_host_mode ();
      mode_selection_menu ()
  | "2" ->
      enter_attendee_mode ();
      mode_selection_menu ()
  | "3" ->
      print_endline "\nGoodbye!";
      exit 0
  | _ ->
      print_endline "Invalid option. Please try again.";
      mode_selection_menu ()

let run () =
  print_endline "\nWelcome to the Calendar Application!";
  mode_selection_menu ()
