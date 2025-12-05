val hash_password : string -> string
(** Generate a salt for the provided password *)

val verify_password : string -> string -> bool
(** Check if a written password matches a stored salt *)

val user_exists : string -> bool
(** Return [true] if a user already exists *)

val create_user : string -> string -> (string, string) result
(** Create new user record with the supplied password *)

val verify_user_password : string -> string -> bool
(** Validate a username and password against stored credentials *)

val host_exists : unit -> bool
(** helper for checking existence of host account *)
