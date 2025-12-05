(** Generate a salt for the provided  password *)
val hash_password : string -> string

(** Check if a written password matches a stored salt *)
val verify_password : string -> string -> bool

(** Return [true] if a user already exists *)
val user_exists : string -> bool

(** Create new user record with the supplied password *)
val create_user : string -> string -> (string, string) result

(** Validate a username and password against stored credentials *)
val verify_user_password : string -> string -> bool

(** helper for checking existence of host account *)
val host_exists : unit -> bool
