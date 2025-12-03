type time = {
  hours : int;
  minutes : int;
}

type date = {
  year : int;
  month : int;
  day : int;
}

type meeting = {
  attendee_name : string;
  date : date;
  start_time : time;
  end_time : time;
}

type role = Host | Attendee

type user = {
  username : string;
  password_hash : string;
  role : role;
}

(* String formatting functions *)
let string_of_time t = Printf.sprintf "%02d:%02d" t.hours t.minutes
let string_of_date d = Printf.sprintf "%04d-%02d-%02d" d.year d.month d.day

let string_of_meeting m =
  Printf.sprintf "%s on %s from %s to %s" m.attendee_name
    (string_of_date m.date)
    (string_of_time m.start_time)
    (string_of_time m.end_time)

(* Convert time to minutes since midnight for easy comparison *)
let time_to_minutes t = (t.hours * 60) + t.minutes

(* Compare two times *)
let compare_time t1 t2 = compare (time_to_minutes t1) (time_to_minutes t2)

(* Compare two dates *)
let compare_date d1 d2 =
  match compare d1.year d2.year with
  | 0 -> (
      match compare d1.month d2.month with
      | 0 -> compare d1.day d2.day
      | x -> x)
  | x -> x

(* Check if a time interval is valid *)
let is_valid_interval start_time end_time =
  compare_time start_time end_time < 0

(* Check if two meetings overlap *)
let meetings_overlap m1 m2 =
  if compare_date m1.date m2.date <> 0 then false
  else
    let m1_start = time_to_minutes m1.start_time in
    let m1_end = time_to_minutes m1.end_time in
    let m2_start = time_to_minutes m2.start_time in
    let m2_end = time_to_minutes m2.end_time in
    not (m1_end <= m2_start || m2_end <= m1_start)
