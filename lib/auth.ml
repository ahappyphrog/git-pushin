open Yojson.Basic.Util

let passwords_file = "passwords.json"

(* Generate a random salt *)
let generate_salt () =
  let random_bytes = String.init 16 (fun _ -> Char.chr (Random.int 256)) in
  Digest.string random_bytes |> Digest.to_hex

(* Hash a password with a salt *)
let hash_password password =
  let salt = generate_salt () in
  let hash = Digest.string (salt ^ password) |> Digest.to_hex in
  salt ^ ":" ^ hash

(* Verify a password against a stored hash *)
let verify_password stored_hash password =
  match String.split_on_char ':' stored_hash with
  | salt :: hash :: _ ->
      let computed_hash = Digest.string (salt ^ password) |> Digest.to_hex in
      computed_hash = hash
  | _ -> false

(* Load all users and their hashed passwords from file *)
let load_passwords () =
  if Sys.file_exists passwords_file then
    try
      let json = Yojson.Basic.from_file passwords_file in
      json |> to_assoc |> List.map (fun (name, pass) -> (name, to_string pass))
    with _ -> []
  else []

(* Save all users and passwords to file *)
let save_passwords users =
  let json =
    `Assoc (List.map (fun (name, pass) -> (name, `String pass)) users)
  in
  Yojson.Basic.to_file passwords_file json

(* Check if a user exists *)
let user_exists username =
  let users = load_passwords () in
  List.exists (fun (name, _) -> name = username) users

(* Create a new user with a password *)
let create_user username password =
  let users = load_passwords () in
  if user_exists username then Error "User already exists"
  else
    let hashed = hash_password password in
    let updated = (username, hashed) :: users in
    save_passwords updated;
    Ok "User created successfully"

(* Verify a user's password (by username) *)
let verify_user_password username password =
  let users = load_passwords () in
  match List.find_opt (fun (name, _) -> name = username) users with
  | Some (_, stored_hash) -> verify_password stored_hash password
  | None -> false

(* Check if host exists *)
let host_exists () = user_exists "host"
