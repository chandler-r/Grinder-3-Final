type level

exception GameOver

val start_level : unit -> level
val incr_level : level -> unit

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
