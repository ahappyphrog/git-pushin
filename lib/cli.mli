(** Terminal user interface for scheduling and viewing meetings. *)

open Types

val read_time : string -> time
(** Prompt for a time using the given message. Re-prompts until valid. *)

val read_date : string -> date
(** Prompt for a date using the given message. Re-prompts until valid. *)

val schedule_meeting_interactive : unit -> unit
(** Walk through scheduling a meeting interactively. *)

val view_attendee_meetings : unit -> unit
(** Show meetings filtered by attendee name. *)

val view_all_meetings : unit -> unit
(** Show all scheduled meetings (host view). *)

val main_menu : unit -> unit
(** Present the main menu loop. *)

val run : unit -> unit
(** Entry point for the CLI experience. *)
