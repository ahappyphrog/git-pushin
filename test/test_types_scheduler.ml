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
  assert_equal ~printer:(fun x -> x) "09:05" (string_of_time (mk_time 9 5));
  assert_equal ~printer:(fun x -> x) "23:59" (string_of_time (mk_time 23 59));
  assert_equal ~printer:(fun x -> x) "2024-12-31" (string_of_date (mk_date 2024 12 31));
  assert_equal ~printer:(fun x -> x) "1900-01-01" (string_of_date (mk_date 1900 1 1))

let test_compare_date _ =
  let d1 = mk_date 2024 1 1 in
  let d2 = mk_date 2024 1 2 in
  let d3 = mk_date 2023 12 31 in
  assert_bool "d1 < d2" (compare_date d1 d2 < 0);
  assert_bool "d2 > d1" (compare_date d2 d1 > 0);
  assert_bool "d3 < d1" (compare_date d3 d1 < 0);
  assert_equal ~printer:string_of_int 0 (compare_date d1 d1)

let test_is_valid_interval _ =
  assert_bool "9:00 -> 10:00 should be valid"
    (is_valid_interval (mk_time 9 0) (mk_time 10 0));
  assert_bool "10:30 -> 10:30 should be invalid"
    (not (is_valid_interval (mk_time 10 30) (mk_time 10 30)));
  assert_bool "11:00 -> 10:00 should be invalid"
    (not (is_valid_interval (mk_time 11 0) (mk_time 10 0)));
  assert_bool "midnight to 23:59 should be valid"
    (is_valid_interval (mk_time 0 0) (mk_time 23 59))

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
      assert_equal ~printer:string_of_int 2 (List.length all);
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

let test_time_edge_cases _ =
  assert_equal ~printer:(fun x -> x) "00:00" (string_of_time (mk_time 0 0));
  assert_equal ~printer:(fun x -> x) "00:01" (string_of_time (mk_time 0 1));
  assert_equal ~printer:(fun x -> x) "12:00" (string_of_time (mk_time 12 0));
  assert_equal ~printer:(fun x -> x) "23:59" (string_of_time (mk_time 23 59))

let test_compare_time _ =
  let t1 = mk_time 9 0 in
  let t2 = mk_time 9 30 in
  let t3 = mk_time 10 0 in
  assert_bool "9:00 < 9:30" (compare_time t1 t2 < 0);
  assert_bool "9:30 < 10:00" (compare_time t2 t3 < 0);
  assert_bool "10:00 > 9:00" (compare_time t3 t1 > 0);
  assert_equal ~printer:string_of_int 0 (compare_time t1 t1)

let test_time_to_minutes _ =
  assert_equal ~printer:string_of_int 0 (time_to_minutes (mk_time 0 0));
  assert_equal ~printer:string_of_int 60 (time_to_minutes (mk_time 1 0));
  assert_equal ~printer:string_of_int 90 (time_to_minutes (mk_time 1 30));
  assert_equal ~printer:string_of_int 1439 (time_to_minutes (mk_time 23 59))

let test_date_month_boundaries _ =
  let jan31 = mk_date 2024 1 31 in
  let feb1 = mk_date 2024 2 1 in
  let dec31 = mk_date 2024 12 31 in
  let jan1_next = mk_date 2025 1 1 in
  assert_bool "Jan 31 < Feb 1" (compare_date jan31 feb1 < 0);
  assert_bool "Dec 31 < Jan 1 next year" (compare_date dec31 jan1_next < 0)

let test_date_leap_year _ =
  let feb28_2024 = mk_date 2024 2 28 in
  let feb29_2024 = mk_date 2024 2 29 in
  let mar1_2024 = mk_date 2024 3 1 in
  assert_bool "Feb 28 < Feb 29 (leap year)" (compare_date feb28_2024 feb29_2024 < 0);
  assert_bool "Feb 29 < Mar 1 (leap year)" (compare_date feb29_2024 mar1_2024 < 0)

let test_meeting_overlap_same_times _ =
  let date = mk_date 2024 5 10 in
  let m1 = mk_meeting "alice" date (mk_time 9 0) (mk_time 10 0) in
  let m2 = mk_meeting "bob" date (mk_time 9 0) (mk_time 10 0) in
  assert_bool "identical time slots should overlap" (meetings_overlap m1 m2)

let test_meeting_overlap_containment _ =
  let date = mk_date 2024 5 10 in
  let outer = mk_meeting "alice" date (mk_time 9 0) (mk_time 12 0) in
  let inner = mk_meeting "bob" date (mk_time 10 0) (mk_time 11 0) in
  assert_bool "inner meeting should overlap with outer" (meetings_overlap outer inner);
  assert_bool "outer meeting should overlap with inner" (meetings_overlap inner outer)

let test_meeting_overlap_partial _ =
  let date = mk_date 2024 5 10 in
  let m1 = mk_meeting "alice" date (mk_time 9 0) (mk_time 10 30) in
  let m2 = mk_meeting "bob" date (mk_time 10 0) (mk_time 11 0) in
  assert_bool "partial overlap should be detected" (meetings_overlap m1 m2)

let test_participants_sharing _ =
  let date = mk_date 2024 6 1 in
  let m1 = mk_meeting ~organizer:"alice" "bob" date (mk_time 9 0) (mk_time 10 0) in
  let m2_shared_organizer = mk_meeting ~organizer:"alice" "carol" date (mk_time 9 0) (mk_time 10 0) in
  let m3_shared_attendee = mk_meeting ~organizer:"dave" "bob" date (mk_time 9 0) (mk_time 10 0) in
  let m4_no_share = mk_meeting ~organizer:"eve" "frank" date (mk_time 9 0) (mk_time 10 0) in
  assert_bool "shared organizer" (Scheduler.share_participant m1 m2_shared_organizer);
  assert_bool "shared attendee" (Scheduler.share_participant m1 m3_shared_attendee);
  assert_bool "no shared participants" (not (Scheduler.share_participant m1 m4_no_share))

let test_schedule_to_empty_list _ =
  with_tmp_dir "empty_schedule" @@ fun () ->
  let meeting = mk_meeting "first" (mk_date 2024 8 1) (mk_time 9 0) (mk_time 10 0) in
  match Scheduler.schedule_meeting meeting with
  | Ok _ ->
      let all = Storage.get_all_meetings () in
      assert_equal ~printer:string_of_int 1 (List.length all)
  | Error _ -> assert_failure "Should schedule to empty list"

let test_schedule_multiple_sequential _ =
  with_tmp_dir "sequential" @@ fun () ->
  let date = mk_date 2024 8 15 in
  let m1 = mk_meeting ~organizer:"host" "alice" date (mk_time 9 0) (mk_time 10 0) in
  let m2 = mk_meeting ~organizer:"host" "bob" date (mk_time 10 0) (mk_time 11 0) in
  let m3 = mk_meeting ~organizer:"host" "carol" date (mk_time 11 0) (mk_time 12 0) in
  (match Scheduler.schedule_meeting m1 with
   | Ok _ -> ()
   | Error e -> assert_failure ("m1 failed: " ^ e));
  (match Scheduler.schedule_meeting m2 with
   | Ok _ -> ()
   | Error e -> assert_failure ("m2 failed: " ^ e));
  (match Scheduler.schedule_meeting m3 with
   | Ok _ -> ()
   | Error e -> assert_failure ("m3 failed: " ^ e));
  let all = Storage.get_all_meetings () in
  assert_equal ~printer:string_of_int 3 (List.length all)

let test_schedule_same_person_conflict _ =
  with_tmp_dir "self_conflict" @@ fun () ->
  let date = mk_date 2024 9 1 in
  let m1 = mk_meeting ~organizer:"alice" "bob" date (mk_time 9 0) (mk_time 10 0) in
  let m2 = mk_meeting ~organizer:"alice" "carol" date (mk_time 9 30) (mk_time 10 30) in
  Storage.save_meetings [ m1 ];
  match Scheduler.schedule_meeting m2 with
  | Error _ -> ()
  | Ok _ -> assert_failure "Same organizer should have conflict"

let test_meeting_string_format _ =
  let meeting = mk_meeting ~organizer:"alice" "bob" (mk_date 2024 10 5) (mk_time 14 30) (mk_time 15 45) in
  let str = string_of_meeting meeting in
  assert_bool "should contain organizer" (String.length str > 0);
  assert_bool "should contain alice" (Str.string_match (Str.regexp ".*alice.*") str 0);
  assert_bool "should contain bob" (Str.string_match (Str.regexp ".*bob.*") str 0);
  assert_bool "should contain date" (Str.string_match (Str.regexp ".*2024-10-05.*") str 0)

let test_cross_day_no_overlap _ =
  let m1 = mk_meeting "alice" (mk_date 2024 10 1) (mk_time 23 0) (mk_time 23 59) in
  let m2 = mk_meeting "bob" (mk_date 2024 10 2) (mk_time 0 0) (mk_time 1 0) in
  assert_bool "meetings on different days should not overlap"
    (not (meetings_overlap m1 m2))

let test_time_json_roundtrip _ =
  let time = mk_time 14 35 in
  let json = Storage.time_to_json time in
  let roundtrip = Storage.json_to_time json in
  assert_equal ~printer:string_of_time time roundtrip

let test_date_json_roundtrip _ =
  let date = mk_date 2024 11 20 in
  let json = Storage.date_to_json date in
  let roundtrip = Storage.json_to_date json in
  assert_equal ~printer:string_of_date date roundtrip

let test_very_long_meeting _ =
  let start = mk_time 0 0 in
  let end_time = mk_time 23 59 in
  assert_bool "all-day meeting should be valid" (is_valid_interval start end_time)

let test_one_minute_meeting _ =
  let start = mk_time 10 0 in
  let end_time = mk_time 10 1 in
  assert_bool "1-minute meeting should be valid" (is_valid_interval start end_time)

let test_meeting_at_midnight _ =
  let date = mk_date 2024 12 1 in
  let meeting = mk_meeting "alice" date (mk_time 0 0) (mk_time 1 0) in
  assert_equal ~printer:(fun x -> x) "alice" meeting.attendee_name;
  assert_equal ~printer:string_of_int 0 meeting.start_time.hours

let test_meeting_ending_at_23_59 _ =
  let date = mk_date 2024 12 1 in
  let meeting = mk_meeting "bob" date (mk_time 23 0) (mk_time 23 59) in
  assert_equal ~printer:string_of_int 23 meeting.end_time.hours;
  assert_equal ~printer:string_of_int 59 meeting.end_time.minutes

let test_year_boundaries _ =
  let dec31_2023 = mk_date 2023 12 31 in
  let jan1_2024 = mk_date 2024 1 1 in
  let dec31_2024 = mk_date 2024 12 31 in
  assert_bool "2023-12-31 < 2024-01-01" (compare_date dec31_2023 jan1_2024 < 0);
  assert_bool "2024-01-01 < 2024-12-31" (compare_date jan1_2024 dec31_2024 < 0)

let test_same_month_different_years _ =
  let jan_2024 = mk_date 2024 1 15 in
  let jan_2025 = mk_date 2025 1 15 in
  assert_bool "2024-01-15 < 2025-01-15" (compare_date jan_2024 jan_2025 < 0)

let test_multiple_overlapping_conflicts _ =
  with_tmp_dir "multi_conflict" @@ fun () ->
  let date = mk_date 2024 11 1 in
  let m1 = mk_meeting ~organizer:"host" "alice" date (mk_time 9 0) (mk_time 10 0) in
  let m2 = mk_meeting ~organizer:"host" "bob" date (mk_time 9 15) (mk_time 9 45) in
  let m3 = mk_meeting ~organizer:"host" "carol" date (mk_time 9 30) (mk_time 10 30) in
  Storage.save_meetings [ m1; m2 ];
  match Scheduler.schedule_meeting m3 with
  | Error _ -> ()
  | Ok _ -> assert_failure "Should conflict with existing meetings"

let test_attendee_conflict _ =
  with_tmp_dir "attendee_conflict" @@ fun () ->
  let date = mk_date 2024 11 15 in
  let m1 = mk_meeting ~organizer:"alice" "bob" date (mk_time 10 0) (mk_time 11 0) in
  let m2 = mk_meeting ~organizer:"carol" "bob" date (mk_time 10 30) (mk_time 11 30) in
  Storage.save_meetings [ m1 ];
  match Scheduler.schedule_meeting m2 with
  | Error _ -> ()
  | Ok _ -> assert_failure "Attendee bob should have conflict"

let test_meeting_overlap_one_minute _ =
  let date = mk_date 2024 11 20 in
  let m1 = mk_meeting "alice" date (mk_time 10 0) (mk_time 11 0) in
  let m2 = mk_meeting "bob" date (mk_time 10 59) (mk_time 12 0) in
  assert_bool "one minute overlap should be detected" (meetings_overlap m1 m2)

let test_meeting_no_overlap_one_minute_gap _ =
  let date = mk_date 2024 11 20 in
  let m1 = mk_meeting "alice" date (mk_time 10 0) (mk_time 11 0) in
  let m2 = mk_meeting "bob" date (mk_time 11 1) (mk_time 12 0) in
  assert_bool "one minute gap should not overlap" (not (meetings_overlap m1 m2))

let test_date_formatting_edge_cases _ =
  assert_equal ~printer:(fun x -> x) "0001-01-01" (string_of_date (mk_date 1 1 1));
  assert_equal ~printer:(fun x -> x) "9999-12-31" (string_of_date (mk_date 9999 12 31))

let test_compare_date_same_year_different_months _ =
  let jan = mk_date 2024 1 15 in
  let feb = mk_date 2024 2 15 in
  let dec = mk_date 2024 12 15 in
  assert_bool "Jan < Feb" (compare_date jan feb < 0);
  assert_bool "Feb < Dec" (compare_date feb dec < 0);
  assert_bool "Jan < Dec" (compare_date jan dec < 0)

let test_compare_date_same_year_month_different_days _ =
  let day1 = mk_date 2024 5 1 in
  let day15 = mk_date 2024 5 15 in
  let day31 = mk_date 2024 5 31 in
  assert_bool "day 1 < day 15" (compare_date day1 day15 < 0);
  assert_bool "day 15 < day 31" (compare_date day15 day31 < 0)

let test_has_conflict_with_empty_list _ =
  let date = mk_date 2024 12 1 in
  let meeting = mk_meeting ~organizer:"alice" "bob" date (mk_time 9 0) (mk_time 10 0) in
  assert_bool "no conflict with empty list" (not (Scheduler.has_conflict meeting []))

let test_has_conflict_with_many_meetings _ =
  let date = mk_date 2024 12 5 in
  let existing = List.init 20 (fun i ->
    mk_meeting ~organizer:"host" ("user" ^ string_of_int i) date
      (mk_time (i mod 10 + 8) 0)
      (mk_time (i mod 10 + 9) 0)
  ) in
  let conflicting = mk_meeting ~organizer:"host" "newuser" date (mk_time 10 30) (mk_time 11 30) in
  let non_conflicting = mk_meeting ~organizer:"other" "newuser" date (mk_time 10 30) (mk_time 11 30) in
  assert_bool "should find conflict in many meetings"
    (Scheduler.has_conflict conflicting existing);
  assert_bool "should not conflict with different participants"
    (not (Scheduler.has_conflict non_conflicting existing))

let test_meeting_string_contains_all_info _ =
  let meeting = mk_meeting ~organizer:"organizer" "attendee"
                  (mk_date 2024 6 15) (mk_time 9 30) (mk_time 10 45) in
  let str = string_of_meeting meeting in
  assert_bool "contains organizer" (Str.string_match (Str.regexp ".*organizer.*") str 0);
  assert_bool "contains attendee" (Str.string_match (Str.regexp ".*attendee.*") str 0);
  assert_bool "contains date" (Str.string_match (Str.regexp ".*2024-06-15.*") str 0);
  assert_bool "contains start time" (Str.string_match (Str.regexp ".*09:30.*") str 0);
  assert_bool "contains end time" (Str.string_match (Str.regexp ".*10:45.*") str 0)

let test_time_minutes_boundary_values _ =
  assert_equal ~printer:string_of_int 0 (time_to_minutes (mk_time 0 0));
  assert_equal ~printer:string_of_int 1 (time_to_minutes (mk_time 0 1));
  assert_equal ~printer:string_of_int 59 (time_to_minutes (mk_time 0 59));
  assert_equal ~printer:string_of_int 1439 (time_to_minutes (mk_time 23 59))

let test_compare_time_boundary _ =
  let t_00_00 = mk_time 0 0 in
  let t_00_01 = mk_time 0 1 in
  let t_23_58 = mk_time 23 58 in
  let t_23_59 = mk_time 23 59 in
  assert_bool "0:00 < 0:01" (compare_time t_00_00 t_00_01 < 0);
  assert_bool "23:58 < 23:59" (compare_time t_23_58 t_23_59 < 0)

let test_share_participant_both_ways _ =
  let date = mk_date 2024 8 1 in
  let m1 = mk_meeting ~organizer:"alice" "bob" date (mk_time 9 0) (mk_time 10 0) in
  let m2 = mk_meeting ~organizer:"bob" "carol" date (mk_time 9 0) (mk_time 10 0) in
  assert_bool "bob is in both meetings" (Scheduler.share_participant m1 m2);
  assert_bool "symmetric" (Scheduler.share_participant m2 m1)

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
         "time_edge_cases" >:: test_time_edge_cases;
         "compare_time" >:: test_compare_time;
         "time_to_minutes" >:: test_time_to_minutes;
         "date_month_boundaries" >:: test_date_month_boundaries;
         "date_leap_year" >:: test_date_leap_year;
         "meeting_overlap_same_times" >:: test_meeting_overlap_same_times;
         "meeting_overlap_containment" >:: test_meeting_overlap_containment;
         "meeting_overlap_partial" >:: test_meeting_overlap_partial;
         "participants_sharing" >:: test_participants_sharing;
         "schedule_to_empty_list" >:: test_schedule_to_empty_list;
         "schedule_multiple_sequential" >:: test_schedule_multiple_sequential;
         "schedule_same_person_conflict" >:: test_schedule_same_person_conflict;
         "meeting_string_format" >:: test_meeting_string_format;
         "cross_day_no_overlap" >:: test_cross_day_no_overlap;
         "time_json_roundtrip" >:: test_time_json_roundtrip;
         "date_json_roundtrip" >:: test_date_json_roundtrip;
         "very_long_meeting" >:: test_very_long_meeting;
         "one_minute_meeting" >:: test_one_minute_meeting;
         "meeting_at_midnight" >:: test_meeting_at_midnight;
         "meeting_ending_at_23_59" >:: test_meeting_ending_at_23_59;
         "year_boundaries" >:: test_year_boundaries;
         "same_month_different_years" >:: test_same_month_different_years;
         "multiple_overlapping_conflicts" >:: test_multiple_overlapping_conflicts;
         "attendee_conflict" >:: test_attendee_conflict;
         "meeting_overlap_one_minute" >:: test_meeting_overlap_one_minute;
         "meeting_no_overlap_one_minute_gap" >:: test_meeting_no_overlap_one_minute_gap;
         "date_formatting_edge_cases" >:: test_date_formatting_edge_cases;
         "compare_date_same_year_different_months" >:: test_compare_date_same_year_different_months;
         "compare_date_same_year_month_different_days" >:: test_compare_date_same_year_month_different_days;
         "has_conflict_with_empty_list" >:: test_has_conflict_with_empty_list;
         "has_conflict_with_many_meetings" >:: test_has_conflict_with_many_meetings;
         "meeting_string_contains_all_info" >:: test_meeting_string_contains_all_info;
         "time_minutes_boundary_values" >:: test_time_minutes_boundary_values;
         "compare_time_boundary" >:: test_compare_time_boundary;
         "share_participant_both_ways" >:: test_share_participant_both_ways;
       ]

let () = run_test_tt_main suite
