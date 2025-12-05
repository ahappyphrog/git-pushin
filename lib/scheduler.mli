(** Meeting scheduling logic and validation. *)

open Types

val share_participant : meeting -> meeting -> bool
(** [share_participant m1 m2] is [true] if the meetings involve any of the same
    people. *)

val has_conflict : meeting -> meeting list -> bool
(** Determine whether [meeting] overlaps any meeting in the provided list. *)

val schedule_meeting : meeting -> (string, string) result
(** Validate and persist a meeting, returning a status message or an error
    explanation. *)
