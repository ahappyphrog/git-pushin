open Types

let participants meeting = [ meeting.organizer_name; meeting.attendee_name ]

let share_participant m1 m2 =
  List.exists
    (fun name -> List.exists (( = ) name) (participants m2))
    (participants m1)

let has_conflict new_meeting existing_meetings =
  List.exists
    (fun m -> share_participant new_meeting m && meetings_overlap new_meeting m)
    existing_meetings

let schedule_meeting meeting =
  let existing = Storage.load_meetings () in
  if not (is_valid_interval meeting.start_time meeting.end_time) then
    Error "Invalid time interval: start time must be before end time"
  else if has_conflict meeting existing then
    Error "Time slot conflicts with an existing meeting"
  else (
    Storage.add_meeting meeting;
    Ok "Meeting scheduled successfully")
