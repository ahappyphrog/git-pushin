open OUnit2
open Types

let safe_remove path = try Sys.remove path with _ -> ()

let with_tmp_dir prefix f =
  let dir = Filename.temp_file prefix "dir" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  let cwd = Sys.getcwd () in
  Sys.chdir dir;
  let cleanup () =
    Sys.chdir cwd;
    Array.iter (fun file -> safe_remove (Filename.concat dir file)) (Sys.readdir dir);
    try Unix.rmdir dir with _ -> ()
  in
  match f () with
  | v ->
      cleanup ();
      v
  | exception e ->
      cleanup ();
      raise e

let test_hash_verification _ =
  let hash = Auth.hash_password "supersecret" in
  assert_bool "password should verify" (Auth.verify_password hash "supersecret");
  assert_bool "wrong password should fail" (not (Auth.verify_password hash "badpass"))

let test_hash_salt_uniqueness _ =
  let h1 = Auth.hash_password "samepassword" in
  let h2 = Auth.hash_password "samepassword" in
  assert_bool "hashes should differ because of salt" (h1 <> h2);
  let extract_salt h =
    match String.split_on_char ':' h with
    | salt :: _ -> salt
    | _ -> assert_failure "hash format missing salt"
  in
  assert_bool "salts should differ" (extract_salt h1 <> extract_salt h2);
  assert_bool "first still verifies" (Auth.verify_password h1 "samepassword");
  assert_bool "second still verifies" (Auth.verify_password h2 "samepassword")

let test_hash_tampering _ =
  let hash = Auth.hash_password "hardened" in
  let tampered = hash ^ "x" in
  assert_bool "tampered hash fails verification"
    (not (Auth.verify_password tampered "hardened"));
  let tampered_salt =
    match String.split_on_char ':' hash with
    | _salt :: rest ->
        String.concat ":" ("deadbeefdeadbeef" :: rest)
    | _ -> assert_failure "hash format missing salt"
  in
  assert_bool "tampered salt fails verification"
    (not (Auth.verify_password tampered_salt "hardened"))

let mk_time h m = { hours = h; minutes = m }
let mk_date y m d = { year = y; month = m; day = d }

let mk_meeting attendee date start_time end_time =
  { attendee_name = attendee; date; start_time; end_time }

let test_user_lifecycle _ =
  with_tmp_dir "users" @@ fun () ->
  assert_bool "no host initially" (not (Storage.host_exists ()));
  (* create host *)
  (match Storage.add_user "host" "topsecret" Host with
  | Ok u ->
      assert_equal Host u.role;
      assert_bool "host hash is not plain password" (u.password_hash <> "topsecret");
      assert_bool "host now exists" (Storage.host_exists ())
  | Error _ -> assert_failure "expected to create host user");
  (* duplicate username *)
  (match Storage.add_user "host" "newpass" Attendee with
  | Error _ -> ()
  | Ok _ -> assert_failure "duplicate usernames should be rejected");
  (* attendee *)
  (match Storage.add_user "guest" "letmein" Attendee with
  | Ok u -> assert_equal Attendee u.role
  | Error _ -> assert_failure "expected attendee creation");
  (* authentication checks *)
  assert_bool "host authenticates"
    (match Storage.authenticate_user "host" "topsecret" with
    | Some u -> u.role = Host
    | None -> false);
  assert_equal None (Storage.authenticate_user "host" "wrongpass");
  assert_equal None (Storage.authenticate_user "nosuch" "letmein")

let test_meeting_persistence _ =
  with_tmp_dir "meetings" @@ fun () ->
  let date = mk_date 2025 1 1 in
  let m1 = mk_meeting "alice" date (mk_time 9 0) (mk_time 10 0) in
  let m2 = mk_meeting "bob" date (mk_time 10 0) (mk_time 11 0) in
  Storage.add_meeting m1;
  Storage.add_meeting m2;
  let all = Storage.get_all_meetings () in
  assert_equal 2 (List.length all);
  let for_alice = Storage.get_meetings_for_attendee "alice" in
  assert_equal 1 (List.length for_alice);
  assert_equal "alice" (List.hd for_alice).attendee_name;
  let for_bob = Storage.get_meetings_for_attendee "bob" in
  assert_equal 1 (List.length for_bob);
  assert_equal "bob" (List.hd for_bob).attendee_name

let test_user_json_roundtrip _ =
  let user = { username = "host"; password_hash = "hash"; role = Host } in
  let json = Storage.user_to_json user in
  let roundtrip = Storage.json_to_user json in
  assert_equal user roundtrip

let suite =
  "auth_storage_suite"
  >::: [
         "hash_verification" >:: test_hash_verification;
         "hash_salt_uniqueness" >:: test_hash_salt_uniqueness;
         "hash_tampering" >:: test_hash_tampering;
         "user_lifecycle" >:: test_user_lifecycle;
         "meeting_persistence" >:: test_meeting_persistence;
         "user_json_roundtrip" >:: test_user_json_roundtrip;
       ]

let () = run_test_tt_main suite
