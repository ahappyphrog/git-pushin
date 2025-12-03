(** Core data structures and helper utilities for meetings. *)

type time = {
  hours : int;
  minutes : int;
}
(** A 24-hour clock time. *)

type date = {
  year : int;
  month : int;
  day : int;
}
(** A calendar date using ISO components. *)

type meeting = {
  attendee_name : string;
  date : date;
  start_time : time;
  end_time : time;
}
(** A scheduled meeting with an attendee, date, and start/end times. *)

val string_of_time : time -> string
(** Format a [time] as [HH:MM]. *)

val string_of_date : date -> string
(** Format a [date] as [YYYY-MM-DD]. *)

val string_of_meeting : meeting -> string
(** Human-readable summary of a [meeting]. *)

val time_to_minutes : time -> int
(** Convert a [time] into minutes since midnight. *)

val compare_time : time -> time -> int
(** Compare two times using minutes since midnight. *)

val compare_date : date -> date -> int
(** Compare two dates by year, month, then day. *)

val is_valid_interval : time -> time -> bool
(** [is_valid_interval start_time end_time] is [true] when [start_time] occurs before [end_time]. *)

val meetings_overlap : meeting -> meeting -> bool
(** Detect whether two meetings on the same date overlap in time. *)
