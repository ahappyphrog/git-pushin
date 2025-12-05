(** Terminal user interface for scheduling and viewing meetings. *)

open Types

val read_time : string -> time
(** Prompt for a time using the given message. Re-prompts until valid. *)

val read_date : string -> date
(** Prompt for a date using the given message. Re-prompts until valid. *)

val schedule_meeting_for : string -> unit
(** Prompt for details and send a meeting invitation from the provided user. *)

val show_attendee_meetings : string -> unit
(** Display every meeting that belongs to the provided attendee/organizer. *)

val show_all_meetings : unit -> unit
(** Display all meetings visible to the host. *)

val handle_pending_invitations : string -> unit
(** Allow the given user to accept or decline pending invitations. *)

val host_dashboard : unit -> unit
(** Host dashboard with meeting, invitation review, and invitation sending
    controls. *)

val attendee_dashboard : string -> unit
(** Attendee dashboard with meeting views, invitation review, and invitation
    sending. *)

val enter_host_mode : unit -> unit
(** Authenticate as host (creating credentials if needed) and open the
    dashboard. *)

val enter_attendee_mode : unit -> unit
(** Authenticate or create an attendee account and open the dashboard. *)

val mode_selection_menu : unit -> unit
(** Present the initial host/attendee mode chooser. *)

val run : unit -> unit
(** Entry point for the CLI experience. *)
