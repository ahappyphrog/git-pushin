open Types

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
      if response = "y" || response = "Y" then schedule_meeting_interactive ()

(* Main menu *)
let rec main_menu () =
  print_endline "\n========================================";
  print_endline "    Calendar Application";
  print_endline "========================================";
  print_endline "1. Schedule a meeting";
  print_endline "2. Exit";
  print_endline "========================================";
  print_string "Select an option (1-2): ";
  flush stdout;

  let choice = read_line () in
  match choice with
  | "1" ->
      schedule_meeting_interactive ();
      main_menu ()
  | "2" ->
      print_endline "\nGoodbye!";
      exit 0
  | _ ->
      print_endline "Invalid option. Please try again.";
      main_menu ()

let run () =
  print_endline "\nWelcome to the Calendar Application!";
  main_menu ()
