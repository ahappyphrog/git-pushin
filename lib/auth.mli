val hash_password : string -> string
(** Generate a salted hash for the provided plain-text password. *)

val verify_password : string -> string -> bool
(** Check whether a plain-text password matches a stored salted hash. *)

val user_exists : string -> bool
(** Return [true] if a user entry already exists. *)

val create_user : string -> string -> (string, string) result
(** Create a new user record with the supplied password. *)

val verify_user_password : string -> string -> bool
(** Validate a username/password pair against stored credentials. *)

val host_exists : unit -> bool
(** Convenience helper to check for the existence of the host account. *)
