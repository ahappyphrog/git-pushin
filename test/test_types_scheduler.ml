open OUnit2
open Git_pushin.Types
open Git_pushin

let mk_time hours minutes = { hours; minutes }
let mk_date year month day = { year; month; day }
let mk_meeting ?(organizer = "host") attendee date start_time end_time =
  { organizer_name = organizer; attendee_name = attendee; date; start_time; end_time }

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

let assert_meeting_equal expected actual =
  assert_equal ~printer:string_of_meeting expected actual

let test_string_formatting _ =
  assert_equal "09:05" (string_of_time (mk_time 9 5));
  assert_equal "23:59" (string_of_time (mk_time 23 59));
  assert_equal "2024-12-31" (string_of_date (mk_date 2024 12 31));
  assert_equal "1900-01-01" (string_of_date (mk_date 1900 1 1))

let test_compare_date _ =
  let d1 = mk_date 2024 1 1 in
  let d2 = mk_date 2024 1 2 in
  let d3 = mk_date 2023 12 31 in
  assert_bool "d1 < d2" (compare_date d1 d2 < 0);
  assert_bool "d2 > d1" (compare_date d2 d1 > 0);
  assert_bool "d3 < d1" (compare_date d3 d1 < 0);
  assert_equal 0 (compare_date d1 d1)

let test_is_valid_interval _ =
  assert_bool "9:00 -> 10:00 should be valid"
    (is_valid_interval (mk_time 9 0) (mk_time 10 0));
  assert_bool "10:30 -> 10:30 should be invalid"
    (not (is_valid_interval (mk_time 10 30) (mk_time 10 30)));
  assert_bool "11:00 -> 10:00 should be invalid"
    (not (is_valid_interval (mk_time 11 0) (mk_time 10 0)))

let test_meetings_overlap_cases _ =
  let date = mk_date 2024 12 31 in
  let m1 = mk_meeting "alice" date (mk_time 9 0) (mk_time 10 0) in
  let m2 = mk_meeting "bob" date (mk_time 9 30) (mk_time 10 30) in
  let back_to_back = mk_meeting "carol" date (mk_time 10 0) (mk_time 11 0) in
  let other_day =
    mk_meeting "dan" (mk_date 2025 1 1) (mk_time 9 0) (mk_time 10 0)
  in
  assert_bool "overlap expected" (meetings_overlap m1 m2);
  assert_bool "back-to-back should not overlap"
    (not (meetings_overlap m1 back_to_back));
  assert_bool "different date should not overlap"
    (not (meetings_overlap m1 other_day))

let test_has_conflict _ =
  let date = mk_date 2024 5 15 in
  let existing =
    [
      mk_meeting ~organizer:"alice" "bob" date (mk_time 9 0) (mk_time 10 0);
      mk_meeting ~organizer:"carol" "dave" date (mk_time 11 0) (mk_time 12 0);
    ]
  in
  let overlapping =
    mk_meeting ~organizer:"alice" "erin" date (mk_time 9 30) (mk_time 9 45)
  in
  let unrelated_parties =
    mk_meeting ~organizer:"erin" "frank" date (mk_time 9 30) (mk_time 9 45)
  in
  let free_slot =
    mk_meeting ~organizer:"alice" "george" date (mk_time 10 0) (mk_time 10 30)
  in
  assert_bool "overlap for shared participant"
    (Scheduler.has_conflict overlapping existing);
  assert_bool "no conflict for different participants"
    (not (Scheduler.has_conflict unrelated_parties existing));
  assert_bool "allow free slot"
    (not (Scheduler.has_conflict free_slot existing))

let test_schedule_invalid_interval _ =
  with_tmp_dir "sched_invalid" @@ fun () ->
  let bad_meeting =
    mk_meeting "erin" (mk_date 2024 6 1) (mk_time 14 0) (mk_time 13 0)
  in
  match Scheduler.schedule_meeting bad_meeting with
  | Error msg -> assert_bool "error message expected" (String.length msg > 0)
  | Ok _ -> assert_failure "Expected invalid interval error"

let test_schedule_conflict_and_success _ =
  with_tmp_dir "sched_conflict" @@ fun () ->
  let date = mk_date 2024 7 7 in
  let existing = mk_meeting ~organizer:"host" "frank" date (mk_time 9 0) (mk_time 10 0) in
  Storage.save_meetings [ existing ];
  let conflict =
    mk_meeting ~organizer:"host" "grace" date (mk_time 9 15) (mk_time 9 45)
  in
  (match Scheduler.schedule_meeting conflict with
  | Error _ -> ()
  | Ok _ -> assert_failure "Expected conflict error");
  let free_slot =
    mk_meeting ~organizer:"host" "heidi" date (mk_time 10 0) (mk_time 10 30)
  in
  match Scheduler.schedule_meeting free_slot with
  | Ok _ ->
      let all = Storage.get_all_meetings () in
      assert_equal 2 (List.length all);
      assert_bool "scheduled meeting is present"
        (List.exists (fun m -> m.attendee_name = "heidi") all)
  | Error _ -> assert_failure "Expected successful scheduling"

let test_meeting_json_roundtrip _ =
  let meeting =
    mk_meeting "ivan" (mk_date 2024 7 4) (mk_time 8 0) (mk_time 9 15)
  in
  let json = Storage.meeting_to_json meeting in
  let roundtrip = Storage.json_to_meeting json in
  assert_meeting_equal meeting roundtrip

let suite =
  "types_scheduler_suite"
  >::: [
         "string_formatting" >:: test_string_formatting;
         "compare_date" >:: test_compare_date;
         "is_valid_interval" >:: test_is_valid_interval;
         "meetings_overlap_cases" >:: test_meetings_overlap_cases;
         "has_conflict" >:: test_has_conflict;
         "schedule_invalid_interval" >:: test_schedule_invalid_interval;
         "schedule_conflict_and_success" >:: test_schedule_conflict_and_success;
         "meeting_json_roundtrip" >:: test_meeting_json_roundtrip;
       ]

let () = run_test_tt_main suite
