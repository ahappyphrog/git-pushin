open OUnit2
open Git_pushin.Types
open Git_pushin

(* Printer functions for assert_equal *)
let string_of_int_list lst = "[" ^ String.concat "; " (List.map string_of_int lst) ^ "]"
let string_of_option f = function
  | Some x -> "Some (" ^ f x ^ ")"
  | None -> "None"
let string_of_user u = Printf.sprintf "{username=%s; role=%s}" u.username
  (match u.role with Host -> "Host" | Attendee -> "Attendee")
let string_of_role = function Host -> "Host" | Attendee -> "Attendee"

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

let mk_meeting ?(organizer = "host") attendee date start_time end_time =
  { organizer_name = organizer; attendee_name = attendee; date; start_time; end_time }

let test_user_lifecycle _ =
  with_tmp_dir "users" @@ fun () ->
  assert_bool "no host initially" (not (Storage.host_exists ()));
  (* create host *)
  (match Storage.add_user "host" "topsecret" Host with
  | Ok u ->
      assert_equal ~printer:string_of_role Host u.role;
      assert_bool "host hash is not plain password" (u.password_hash <> "topsecret");
      assert_bool "host now exists" (Storage.host_exists ())
  | Error _ -> assert_failure "expected to create host user");
  (match Storage.add_user "host" "newpass" Attendee with
  | Error _ -> ()
  | Ok _ -> assert_failure "duplicate usernames should be rejected");
  (match Storage.add_user "guest" "letmein" Attendee with
  | Ok u -> assert_equal ~printer:string_of_role Attendee u.role
  | Error _ -> assert_failure "expected attendee creation");
  assert_bool "host authenticates"
    (match Storage.authenticate_user "host" "topsecret" with
    | Some u -> u.role = Host
    | None -> false);
  assert_equal ~printer:(string_of_option string_of_user) None (Storage.authenticate_user "host" "wrongpass");
  assert_equal ~printer:(string_of_option string_of_user) None (Storage.authenticate_user "nosuch" "letmein")

let test_meeting_persistence _ =
  with_tmp_dir "meetings" @@ fun () ->
  let date = mk_date 2025 1 1 in
  let m1 = mk_meeting "alice" date (mk_time 9 0) (mk_time 10 0) in
  let m2 = mk_meeting "bob" date (mk_time 10 0) (mk_time 11 0) in
  Storage.add_meeting m1;
  Storage.add_meeting m2;
  let all = Storage.get_all_meetings () in
  assert_equal ~printer:string_of_int 2 (List.length all);
  let for_alice = Storage.get_meetings_for_user "alice" in
  assert_equal ~printer:string_of_int 1 (List.length for_alice);
  assert_equal ~printer:(fun x -> x) "alice" (List.hd for_alice).attendee_name;
  let for_bob = Storage.get_meetings_for_user "bob" in
  assert_equal ~printer:string_of_int 1 (List.length for_bob);
  assert_equal ~printer:(fun x -> x) "bob" (List.hd for_bob).attendee_name

let test_user_json_roundtrip _ =
  let user = { username = "host"; password_hash = "hash"; role = Host } in
  let json = Storage.user_to_json user in
  let roundtrip = Storage.json_to_user json in
  assert_equal ~printer:string_of_user user roundtrip

let test_empty_password _ =
  let hash = Auth.hash_password "" in
  assert_bool "empty password should hash" (String.length hash > 0);
  assert_bool "empty password should verify" (Auth.verify_password hash "");
  assert_bool "non-empty should not match" (not (Auth.verify_password hash "x"))

let test_long_password _ =
  let long_pwd = String.make 1000 'a' in
  let hash = Auth.hash_password long_pwd in
  assert_bool "long password should verify" (Auth.verify_password hash long_pwd);
  assert_bool "truncated should not match"
    (not (Auth.verify_password hash (String.make 999 'a')))

let test_special_chars_password _ =
  let special = "p@ssw0rd!#$%^&*(){}[]|\\:;\"'<>,.?/~`" in
  let hash = Auth.hash_password special in
  assert_bool "special chars should verify" (Auth.verify_password hash special);
  assert_bool "similar but different should fail"
    (not (Auth.verify_password hash "p@ssw0rd"))

let test_hash_format _ =
  let hash = Auth.hash_password "test" in
  let parts = String.split_on_char ':' hash in
  assert_equal ~printer:string_of_int 2 (List.length parts);
  let salt = List.hd parts in
  assert_bool "salt should be hex string (32 chars)" (String.length salt = 32)

let test_special_chars_username _ =
  with_tmp_dir "special_username" @@ fun () ->
  match Storage.add_user "user@host.com" "pass" Attendee with
  | Ok u -> assert_equal ~printer:(fun x -> x) "user@host.com" u.username
  | Error _ -> assert_failure "special chars in username should work"

let test_empty_username _ =
  with_tmp_dir "empty_username" @@ fun () ->
  match Storage.add_user "" "password" Attendee with
  | Ok u -> assert_equal ~printer:(fun x -> x) "" u.username
  | Error _ -> assert_failure "empty username should be allowed by storage"

let test_meetings_for_nonexistent_user _ =
  with_tmp_dir "nonexistent" @@ fun () ->
  let meetings = Storage.get_meetings_for_user "nosuchuser" in
  assert_equal ~printer:string_of_int 0 (List.length meetings)

let test_multiple_users _ =
  with_tmp_dir "multiuser" @@ fun () ->
  let _ = Storage.add_user "alice" "pass1" Host in
  let _ = Storage.add_user "bob" "pass2" Attendee in
  let _ = Storage.add_user "carol" "pass3" Attendee in
  assert_bool "alice authenticates"
    (match Storage.authenticate_user "alice" "pass1" with
     | Some u -> u.role = Host
     | None -> false);
  assert_bool "bob authenticates"
    (match Storage.authenticate_user "bob" "pass2" with
     | Some u -> u.role = Attendee
     | None -> false);
  assert_bool "carol authenticates"
    (match Storage.authenticate_user "carol" "pass3" with
     | Some u -> u.role = Attendee
     | None -> false)

let test_invitation_workflow _ =
  with_tmp_dir "invitations" @@ fun () ->
  let date = mk_date 2025 3 15 in
  let inv1 = mk_meeting ~organizer:"alice" "bob" date (mk_time 10 0) (mk_time 11 0) in
  let inv2 = mk_meeting ~organizer:"alice" "carol" date (mk_time 14 0) (mk_time 15 0) in
  let inv3 = mk_meeting ~organizer:"dave" "bob" date (mk_time 16 0) (mk_time 17 0) in
  Storage.add_invitation inv1;
  Storage.add_invitation inv2;
  Storage.add_invitation inv3;
  let bob_invites = Storage.get_invitations_for_user "bob" in
  assert_equal ~printer:string_of_int 2 (List.length bob_invites);
  let carol_invites = Storage.get_invitations_for_user "carol" in
  assert_equal ~printer:string_of_int 1 (List.length carol_invites);
  Storage.remove_invitation (fun inv -> inv.attendee_name = "bob");
  let bob_after = Storage.get_invitations_for_user "bob" in
  assert_equal ~printer:string_of_int 0 (List.length bob_after);
  let carol_after = Storage.get_invitations_for_user "carol" in
  assert_equal ~printer:string_of_int 1 (List.length carol_after)

let test_meeting_organizer_retrieval _ =
  with_tmp_dir "organizer_meetings" @@ fun () ->
  let date = mk_date 2025 4 1 in
  let m1 = mk_meeting ~organizer:"alice" "bob" date (mk_time 9 0) (mk_time 10 0) in
  let m2 = mk_meeting ~organizer:"alice" "carol" date (mk_time 11 0) (mk_time 12 0) in
  let m3 = mk_meeting ~organizer:"bob" "dave" date (mk_time 13 0) (mk_time 14 0) in
  Storage.add_meeting m1;
  Storage.add_meeting m2;
  Storage.add_meeting m3;
  let alice_meetings = Storage.get_meetings_for_user "alice" in
  assert_equal ~printer:string_of_int 2 (List.length alice_meetings);
  let bob_meetings = Storage.get_meetings_for_user "bob" in
  assert_equal ~printer:string_of_int 2 (List.length bob_meetings)

let test_role_json_roundtrip _ =
  let host_json = Storage.role_to_json Host in
  let attendee_json = Storage.role_to_json Attendee in
  assert_equal ~printer:string_of_role Host (Storage.json_to_role host_json);
  assert_equal ~printer:string_of_role Attendee (Storage.json_to_role attendee_json)

let test_unicode_username _ =
  with_tmp_dir "unicode" @@ fun () ->
  match Storage.add_user "用户名" "password" Attendee with
  | Ok u -> assert_equal ~printer:(fun x -> x) "用户名" u.username
  | Error _ -> assert_failure "unicode username should work"

let test_very_long_username _ =
  with_tmp_dir "long_user" @@ fun () ->
  let long_name = String.make 500 'x' in
  match Storage.add_user long_name "pass" Attendee with
  | Ok u -> assert_equal ~printer:(fun x -> x) long_name u.username
  | Error _ -> assert_failure "long username should work"

let test_authentication_case_sensitivity _ =
  with_tmp_dir "case_test" @@ fun () ->
  let _ = Storage.add_user "Alice" "secret" Attendee in
  assert_bool "exact match should work"
    (match Storage.authenticate_user "Alice" "secret" with
     | Some _ -> true
     | None -> false);
  assert_equal ~printer:(string_of_option string_of_user) None (Storage.authenticate_user "alice" "secret");
  assert_equal ~printer:(string_of_option string_of_user) None (Storage.authenticate_user "ALICE" "secret")

let test_empty_password_authentication _ =
  with_tmp_dir "empty_auth" @@ fun () ->
  let _ = Storage.add_user "user" "" Attendee in
  assert_bool "empty password should authenticate"
    (match Storage.authenticate_user "user" "" with
     | Some _ -> true
     | None -> false);
  assert_equal ~printer:(string_of_option string_of_user) None (Storage.authenticate_user "user" "nonempty")

let test_multiple_host_attempts _ =
  with_tmp_dir "multi_host" @@ fun () ->
  let _ = Storage.add_user "host1" "pass1" Host in
  assert_bool "first host created" (Storage.host_exists ());
  let _ = Storage.add_user "host2" "pass2" Host in
  let users = Storage.load_users () in
  let host_count = List.filter (fun u -> u.role = Host) users |> List.length in
  assert_bool "should have multiple hosts" (host_count = 2)

let test_invitation_json_roundtrip _ =
  let date = mk_date 2025 5 20 in
  let inv = mk_meeting ~organizer:"host" "guest" date (mk_time 15 0) (mk_time 16 0) in
  let json = Storage.meeting_to_json inv in
  let roundtrip = Storage.json_to_meeting json in
  assert_equal ~printer:(fun x -> x) inv.organizer_name roundtrip.organizer_name;
  assert_equal ~printer:(fun x -> x) inv.attendee_name roundtrip.attendee_name;
  assert_equal ~printer:string_of_date inv.date roundtrip.date

let test_storage_many_users _ =
  with_tmp_dir "many_users" @@ fun () ->
  for i = 1 to 50 do
    let username = "user" ^ string_of_int i in
    let _ = Storage.add_user username "pass" Attendee in
    ()
  done;
  let users = Storage.load_users () in
  assert_equal ~printer:string_of_int 50 (List.length users)

let test_storage_many_meetings _ =
  with_tmp_dir "many_meetings" @@ fun () ->
  let date = mk_date 2025 6 1 in
  for i = 0 to 49 do
    let meeting = mk_meeting ("user" ^ string_of_int i) date
                    (mk_time (i mod 14 + 8) 0)
                    (mk_time (i mod 14 + 9) 0) in
    Storage.add_meeting meeting
  done;
  let all = Storage.get_all_meetings () in
  assert_equal ~printer:string_of_int 50 (List.length all)

let test_remove_invitations_complex_predicate _ =
  with_tmp_dir "complex_remove" @@ fun () ->
  let date = mk_date 2025 7 1 in
  let inv1 = mk_meeting ~organizer:"alice" "bob" date (mk_time 9 0) (mk_time 10 0) in
  let inv2 = mk_meeting ~organizer:"alice" "carol" date (mk_time 11 0) (mk_time 12 0) in
  let inv3 = mk_meeting ~organizer:"dave" "bob" date (mk_time 13 0) (mk_time 14 0) in
  Storage.add_invitation inv1;
  Storage.add_invitation inv2;
  Storage.add_invitation inv3;
  Storage.remove_invitation (fun inv ->
    inv.organizer_name = "alice" && inv.start_time.hours = 9);
  let remaining = Storage.load_invitations () in
  assert_equal ~printer:string_of_int 2 (List.length remaining);
  assert_bool "inv1 should be removed"
    (not (List.exists (fun i -> i.attendee_name = "bob" && i.start_time.hours = 9) remaining))

let test_get_invitations_when_none _ =
  with_tmp_dir "no_invites" @@ fun () ->
  let invites = Storage.get_invitations_for_user "nobody" in
  assert_equal ~printer:string_of_int 0 (List.length invites)

let test_salt_randomness _ =
  let salts = List.init 100 (fun _ ->
    let hash = Auth.hash_password "test" in
    match String.split_on_char ':' hash with
    | salt :: _ -> salt
    | _ -> assert_failure "invalid hash format"
  ) in
  let unique_salts = List.sort_uniq String.compare salts in
  assert_bool "should generate diverse salts" (List.length unique_salts > 95)

let test_meeting_retrieval_empty _ =
  with_tmp_dir "empty_meetings" @@ fun () ->
  let meetings = Storage.get_all_meetings () in
  assert_equal ~printer:string_of_int 0 (List.length meetings)

let test_user_role_persistence _ =
  with_tmp_dir "role_persist" @@ fun () ->
  let _ = Storage.add_user "admin" "pass" Host in
  let _ = Storage.add_user "guest" "pass" Attendee in
  let users = Storage.load_users () in
  let admin = List.find (fun u -> u.username = "admin") users in
  let guest = List.find (fun u -> u.username = "guest") users in
  assert_equal ~printer:string_of_role Host admin.role;
  assert_equal ~printer:string_of_role Attendee guest.role

let test_password_with_colon _ =
  let hash = Auth.hash_password "pass:word:with:colons" in
  assert_bool "password with colons should verify"
    (Auth.verify_password hash "pass:word:with:colons");
  assert_bool "wrong password should fail"
    (not (Auth.verify_password hash "pass:word"))

let test_meeting_same_organizer_and_attendee _ =
  with_tmp_dir "self_meeting" @@ fun () ->
  let date = mk_date 2025 8 1 in
  let self_meeting = mk_meeting ~organizer:"alice" "alice" date (mk_time 10 0) (mk_time 11 0) in
  Storage.add_meeting self_meeting;
  let meetings = Storage.get_meetings_for_user "alice" in
  assert_equal ~printer:string_of_int 1 (List.length meetings)

let test_meetings_ordering_preservation _ =
  with_tmp_dir "order" @@ fun () ->
  let date = mk_date 2025 9 1 in
  let m1 = mk_meeting "alice" date (mk_time 9 0) (mk_time 10 0) in
  let m2 = mk_meeting "bob" date (mk_time 10 0) (mk_time 11 0) in
  let m3 = mk_meeting "carol" date (mk_time 11 0) (mk_time 12 0) in
  Storage.add_meeting m1;
  Storage.add_meeting m2;
  Storage.add_meeting m3;
  let all = Storage.get_all_meetings () in
  assert_equal ~printer:(fun x -> x) "carol" (List.hd all).attendee_name

let suite =
  "auth_storage_suite"
  >::: [
         "hash_verification" >:: test_hash_verification;
         "hash_salt_uniqueness" >:: test_hash_salt_uniqueness;
         "hash_tampering" >:: test_hash_tampering;
         "user_lifecycle" >:: test_user_lifecycle;
         "meeting_persistence" >:: test_meeting_persistence;
         "user_json_roundtrip" >:: test_user_json_roundtrip;
         "empty_password" >:: test_empty_password;
         "long_password" >:: test_long_password;
         "special_chars_password" >:: test_special_chars_password;
         "hash_format" >:: test_hash_format;
         "special_chars_username" >:: test_special_chars_username;
         "empty_username" >:: test_empty_username;
         "meetings_for_nonexistent_user" >:: test_meetings_for_nonexistent_user;
         "multiple_users" >:: test_multiple_users;
         "invitation_workflow" >:: test_invitation_workflow;
         "meeting_organizer_retrieval" >:: test_meeting_organizer_retrieval;
         "role_json_roundtrip" >:: test_role_json_roundtrip;
         "unicode_username" >:: test_unicode_username;
         "very_long_username" >:: test_very_long_username;
         "authentication_case_sensitivity" >:: test_authentication_case_sensitivity;
         "empty_password_authentication" >:: test_empty_password_authentication;
         "multiple_host_attempts" >:: test_multiple_host_attempts;
         "invitation_json_roundtrip" >:: test_invitation_json_roundtrip;
         "storage_many_users" >:: test_storage_many_users;
         "storage_many_meetings" >:: test_storage_many_meetings;
         "remove_invitations_complex_predicate" >:: test_remove_invitations_complex_predicate;
         "get_invitations_when_none" >:: test_get_invitations_when_none;
         "salt_randomness" >:: test_salt_randomness;
         "meeting_retrieval_empty" >:: test_meeting_retrieval_empty;
         "user_role_persistence" >:: test_user_role_persistence;
         "password_with_colon" >:: test_password_with_colon;
         "meeting_same_organizer_and_attendee" >:: test_meeting_same_organizer_and_attendee;
         "meetings_ordering_preservation" >:: test_meetings_ordering_preservation;
       ]

let () = run_test_tt_main suite
