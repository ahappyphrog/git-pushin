(** JSON-backed persistence for meetings. *)

open Types

val meetings_file : string
(** File path used for persisting meeting data. *)

val time_to_json : time -> Yojson.Basic.t
(** Serialize a [time] to JSON. *)

val date_to_json : date -> Yojson.Basic.t
(** Serialize a [date] to JSON. *)

val meeting_to_json : meeting -> Yojson.Basic.t
(** Serialize a [meeting] to JSON. *)

val json_to_time : Yojson.Basic.t -> time
(** Parse a JSON value into a [time]. *)

val json_to_date : Yojson.Basic.t -> date
(** Parse a JSON value into a [date]. *)

val json_to_meeting : Yojson.Basic.t -> meeting
(** Parse a JSON value into a [meeting]. *)

val load_meetings : unit -> meeting list
(** Load all meetings from disk. Returns an empty list when the file does not exist or is unreadable. *)

val save_meetings : meeting list -> unit
(** Persist all provided meetings to disk. *)

val add_meeting : meeting -> unit
(** Append a meeting to the persisted set. *)

val get_meetings_for_attendee : string -> meeting list
(** Retrieve all meetings matching the provided attendee name. *)

val get_all_meetings : unit -> meeting list
(** Retrieve all persisted meetings. *)
