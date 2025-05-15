type level
(** The type of a level, indicating the ante and blind the player currently must
    beat. *)

exception GameOver

val start_level : unit -> level
(** [start_level ()] creates the level that a new game starts on. It is always a
    small blind on ante 1. *)

val incr_level : level -> unit
(** [incr_level lev] updates [lev] at the end of a round. If it was a small
    blind, it becomes a big blind. If it was a big blind, it becomes a boss
    blind, and if it was a boss blind, it becomes a small blind with the ante
    increased by 1. *)

val target_score : level -> int
(** [target_score l] is the user's target score to beat a specific level [l]. *)

val end_of_round_bonus : level -> int
(** [end_of_round_bonus l] is the end of round money bonus associated with
    beating level [l]. *)

val ante : level -> int
(** [ante l] is the current ante of level [l]. Will necessarily be between 1 and
    8 (inclusive). *)

val blind : level -> string
(** [blind l] is the current blind of level [l]. Necessarily one of ["Small"],
    ["Big"], or ["Boss"]. *)

val to_string : level -> string
(** string representation of the level. *)

(* val choose_next_blind : level -> unit *)

(* val to_pair : level -> int * string *)
(** gives a pair that is [(ante, blind)] *)

val of_pair : int * string -> level
(** FOR TESTING PURPOSES ONLY. Gives the level for a pair [(ante, blind)].
    Requires [ante] between [1] and [8] (inclusive), [blind] is one of
    ["Small"], ["Big"], ["Boss"]. *)
