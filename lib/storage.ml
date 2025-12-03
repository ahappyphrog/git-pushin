open Types
open Yojson.Basic.Util

let meetings_file = "meetings.json"

(* Convert time to JSON *)
let time_to_json t =
  `Assoc [ ("hours", `Int t.hours); ("minutes", `Int t.minutes) ]

(* Convert date to JSON *)
let date_to_json d =
  `Assoc [ ("year", `Int d.year); ("month", `Int d.month); ("day", `Int d.day) ]

(* Convert meeting to JSON *)
let meeting_to_json m =
  `Assoc
    [
      ("attendee_name", `String m.attendee_name);
      ("date", date_to_json m.date);
      ("start_time", time_to_json m.start_time);
      ("end_time", time_to_json m.end_time);
    ]

(* Convert JSON to time *)
let json_to_time json =
  {
    hours = json |> member "hours" |> to_int;
    minutes = json |> member "minutes" |> to_int;
  }

(* Convert JSON to date *)
let json_to_date json =
  {
    year = json |> member "year" |> to_int;
    month = json |> member "month" |> to_int;
    day = json |> member "day" |> to_int;
  }

(* Convert JSON to meeting *)
let json_to_meeting json =
  {
    attendee_name = json |> member "attendee_name" |> to_string;
    date = json |> member "date" |> json_to_date;
    start_time = json |> member "start_time" |> json_to_time;
    end_time = json |> member "end_time" |> json_to_time;
  }

(* Load all meetings from file *)
let load_meetings () =
  if Sys.file_exists meetings_file then
    try
      let json = Yojson.Basic.from_file meetings_file in
      json |> to_list |> List.map json_to_meeting
    with _ -> []
  else []

(* Save all meetings to file *)
let save_meetings meetings =
  let json = `List (List.map meeting_to_json meetings) in
  Yojson.Basic.to_file meetings_file json

(* Add a new meeting *)
let add_meeting meeting =
  let meetings = load_meetings () in
  let updated = meeting :: meetings in
  save_meetings updated

let get_meetings_for_attendee name =
  let meetings = load_meetings () in
  List.filter (fun m -> m.attendee_name = name) meetings

let get_all_meetings () = load_meetings ()

(* User management *)
let users_file = "users.json"

(* Convert role to JSON *)
let role_to_json = function
  | Host -> `String "Host"
  | Attendee -> `String "Attendee"

(* Convert JSON to role *)
let json_to_role json =
  match to_string json with
  | "Host" -> Host
  | "Attendee" -> Attendee
  | _ -> failwith "Invalid role"

(* Convert user to JSON *)
let user_to_json u =
  `Assoc [
    ("username", `String u.username);
    ("password_hash", `String u.password_hash);
    ("role", role_to_json u.role);
  ]

(* Convert JSON to user *)
let json_to_user json =
  {
    username = json |> member "username" |> to_string;
    password_hash = json |> member "password_hash" |> to_string;
    role = json |> member "role" |> json_to_role;
  }

(* Load all users from file *)
let load_users () =
  if Sys.file_exists users_file then
    try
      let json = Yojson.Basic.from_file users_file in
      json |> to_list |> List.map json_to_user
    with _ -> []
  else []

(* Save all users to file *)
let save_users users =
  let json = `List (List.map user_to_json users) in
  Yojson.Basic.to_file users_file json

(* Check if host exists *)
let host_exists () =
  let users = load_users () in
  List.exists (fun u -> u.role = Host) users

(* Add a new user *)
let add_user username password role =
  let users = load_users () in
  if List.exists (fun u -> u.username = username) users then
    Error "User already exists"
  else
    let password_hash = Auth.hash_password password in
    let user = { username; password_hash; role } in
    save_users (user :: users);
    Ok user

(* Authenticate a user *)
let authenticate_user username password =
  let users = load_users () in
  match List.find_opt (fun u -> u.username = username) users with
  | Some user when Auth.verify_password user.password_hash password -> Some user
  | _ -> None