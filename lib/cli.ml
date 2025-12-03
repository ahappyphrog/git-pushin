open Types

(* Read password without echo - simple version *)
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
  if Auth.user_exists username then (
    let password = read_password "Enter your password" in
    if Auth.verify_user_password username password then (
      print_endline "Authentication successful!";
      true)
    else (
      print_endline "Incorrect password!";
      false))
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

let rec schedule_meeting_interactive () =
  print_endline "\n--- Schedule a Meeting ---";
  print_string "Enter your name: ";
  flush stdout;
  let name = read_line () in

  if authenticate_user name then (
    let date = read_date "Enter date" in
    let start_time = read_time "Enter start time" in
    let end_time = read_time "Enter end time" in

    let meeting = { attendee_name = name; date; start_time; end_time } in
    match Scheduler.schedule_meeting meeting with
    | Ok msg ->
        print_endline ("\n" ^ msg);
        print_endline (string_of_meeting meeting)
    | Error msg ->
        print_endline ("\nError: " ^ msg);
        print_string "Would you like to try again? (y/n): ";
        flush stdout;
        let response = read_line () in
        if response = "y" || response = "Y" then schedule_meeting_interactive ())
  else
    print_endline "Authentication failed. Returning to main menu."

let view_attendee_meetings () =
  print_endline "\n--- View Your Meetings ---";
  print_string "Enter your name: ";
  flush stdout;
  let name = read_line () in

  if authenticate_user name then (
    let meetings = Storage.get_meetings_for_attendee name in
    if List.length meetings = 0 then
      Printf.printf "\nNo meetings found for %s.\n" name
    else (
      Printf.printf "\nMeetings for %s:\n" name;
      List.iter (fun m -> print_endline ("  - " ^ string_of_meeting m)) meetings))
  else
    print_endline "Authentication failed. Returning to main menu."

let view_all_meetings () =
  print_endline "\n--- All Scheduled Meetings (Host View) ---";
  print_endline "This view requires host authentication.\n";
  let password = read_password "Enter host password" in

  if Auth.verify_user_password "host" password then (
    print_endline "Authentication successful!\n";
    let meetings = Storage.get_all_meetings () in
    if List.length meetings = 0 then print_endline "No meetings scheduled yet."
    else (
      Printf.printf "Total meetings: %d\n\n" (List.length meetings);
      List.iter (fun m -> print_endline ("  - " ^ string_of_meeting m)) meetings))
  else
    print_endline "Incorrect host password. Returning to main menu."

(* Main menu *)
let rec main_menu () =
  print_endline "\n========================================";
  print_endline "    Calendar Application";
  print_endline "========================================";
  print_endline "1. Schedule a meeting (Attendee)";
  print_endline "2. View my meetings (Attendee)";
  print_endline "3. View all meetings (Host)";
  print_endline "4. Exit";
  print_endline "========================================";
  print_string "Select an option (1-4): ";
  flush stdout;

  let choice = read_line () in
  match choice with
  | "1" ->
      schedule_meeting_interactive ();
      main_menu ()
  | "2" ->
      view_attendee_meetings ();
      main_menu ()
  | "3" ->
      view_all_meetings ();
      main_menu ()
  | "4" ->
      print_endline "\nGoodbye!";
      exit 0
  | _ ->
      print_endline "Invalid option. Please try again.";
      main_menu ()

let run () =
  print_endline "\nWelcome to the Calendar Application!";
  setup_host_password ();
  main_menu ()