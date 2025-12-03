(** Terminal user interface for scheduling and viewing meetings. *)

open Types

val read_time : string -> time
(** Prompt for a time using the given message. Re-prompts until valid. *)

val read_date : string -> date
(** Prompt for a date using the given message. Re-prompts until valid. *)

val schedule_meeting_for : string -> unit
(** Schedule a meeting for the provided, already-authenticated attendee. *)

val show_attendee_meetings : string -> unit
(** Display every meeting that belongs to the provided attendee. *)

val show_all_meetings : unit -> unit
(** Display all meetings visible to the host. *)

val host_dashboard : unit -> unit
(** Host-specific dashboard that lists meetings and refresh/logout options. *)

val attendee_dashboard : string -> unit
(** Attendee-specific dashboard that lists meetings and actions. *)

val enter_host_mode : unit -> unit
(** Authenticate as host (creating credentials if needed) and open the dashboard. *)

val enter_attendee_mode : unit -> unit
(** Authenticate or create an attendee account and open the dashboard. *)

val mode_selection_menu : unit -> unit
(** Present the initial host/attendee mode chooser. *)

val run : unit -> unit
(** Entry point for the CLI experience. *)
