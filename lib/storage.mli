(** JSON-backed persistence for meetings. *)

open Types

val meetings_file : string
(** File path used for persisting meeting data. *)

val time_to_json : time -> Yojson.Basic.t
(** Serialize a [time] to JSON. *)

val date_to_json : date -> Yojson.Basic.t
(** Serialize a [date] to JSON. *)

val meeting_to_json : meeting -> Yojson.Basic.t
(** Serialize a [meeting] (including organizer/attendee data) to JSON. *)

val json_to_time : Yojson.Basic.t -> time
(** Parse a JSON value into a [time]. *)

val json_to_date : Yojson.Basic.t -> date
(** Parse a JSON value into a [date]. *)

val json_to_meeting : Yojson.Basic.t -> meeting
(** Parse a JSON value into a [meeting], filling legacy data when needed. *)

val load_meetings : unit -> meeting list
(** Load all meetings from disk. Returns an empty list when the file does not
    exist or is unreadable. *)

val save_meetings : meeting list -> unit
(** Persist all provided meetings to disk. *)

val add_meeting : meeting -> unit
(** Append a meeting to the persisted set. *)

val get_meetings_for_user : string -> meeting list
(** Retrieve all meetings where the provided user participates. *)

val get_all_meetings : unit -> meeting list
(** Retrieve all persisted meetings. *)

(** {1 User Management} *)

val users_file : string
(** File path used for persisting user data. *)

val role_to_json : role -> Yojson.Basic.t
(** Serialize a [role] to JSON. *)

val json_to_role : Yojson.Basic.t -> role
(** Parse a JSON value into a [role]. *)

val user_to_json : user -> Yojson.Basic.t
(** Serialize a [user] to JSON. *)

val json_to_user : Yojson.Basic.t -> user
(** Parse a JSON value into a [user]. *)

val load_users : unit -> user list
(** Load all users from disk. Returns an empty list when the file does not exist
    or is unreadable. *)

val save_users : user list -> unit
(** Persist all provided users to disk. *)

val host_exists : unit -> bool
(** Check if a user with Host role exists. *)

val add_user : string -> string -> role -> (user, string) result
(** [add_user username password role] creates a new user with the given
    credentials and role. Returns [Error] if the username already exists. *)

val authenticate_user : string -> string -> user option
(** [authenticate_user username password] verifies credentials and returns the
    user if valid. *)

(** {1 Invitation Management} *)

val invitations_file : string
(** File path used for persisting invitations. *)

val load_invitations : unit -> invitation list
(** Load all pending invitations from disk. *)

val save_invitations : invitation list -> unit
(** Persist all pending invitations to disk. *)

val add_invitation : invitation -> unit
(** Append a new invitation awaiting acceptance. *)

val get_invitations_for_user : string -> invitation list
(** Retrieve invitations awaiting response from the provided invitee. *)

val remove_invitation : (invitation -> bool) -> unit
(** Remove invitations matching the predicate (used for accept/decline). *)
