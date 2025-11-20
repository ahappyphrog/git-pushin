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