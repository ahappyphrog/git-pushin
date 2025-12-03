(** Meeting scheduling logic and validation. *)

open Types

(** Determine if [meeting] overlaps any meeting in the the list *)
val has_conflict : meeting -> meeting list -> bool

(** validate and store a meeting, returning a status message or an error *)
val schedule_meeting : meeting -> (string, string) result
