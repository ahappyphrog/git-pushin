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

(* String formatting functions *)
let string_of_time t = Printf.sprintf "%02d:%02d" t.hours t.minutes
let string_of_date d = Printf.sprintf "%04d-%02d-%02d" d.year d.month d.day

let string_of_meeting m =
  Printf.sprintf "%s on %s from %s to %s" m.attendee_name
    (string_of_date m.date)
    (string_of_time m.start_time)
    (string_of_time m.end_time)
