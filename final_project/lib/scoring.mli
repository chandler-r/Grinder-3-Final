type t
(** The type of a player's score in the current round. *)

type chips
(** The type of some number of chips used for scoring. *)

type mult
(** The type of a score multiplier. *)

exception InvalidScoring
(** Raised when an operation uses a score that is negative, a number of chips
    that is negative, or a multiplier less than 1.0. *)

val get_score : t -> int
(** [get_score score] is the score [score] represented as an integer.*)

val get_chips : chips -> int
(** [get_chips chips] is the number of chips represented by [chips] represented
    as an integer.*)

val get_mult : mult -> float
(** [get_mult mult] is the multiplier [mult] represented as a float.*)

val calculate_chips : Card.t list -> int
(** [calculate_chips played] is the total number of chips earned from playing
    all the cards in [played] at one time. The number of chips is represented as
    an integer. [played] must contain at least 1 card and no more than
    [Hand.play_limit] cards. *)

val calculate_mult : Card.t list -> float
(** [calculate_mult played] is the total multiplier earned from playing all the
    cards in [played] at one time. The multiplier is represented as a float.
    [played] must contain at least 1 card and no more than [Hand.play_limit]
    cards. *)

val score_played_cards : Card.t list -> int
(** [score_played_cards played] is the total score earned from playing all the
    cards in [played] at one time, calculated by multiplying the number of chips
    earned from the hand by the multiplier earned from the hand. The score is
    represented as an integer. [played] must contain at least 1 card and no more
    than [Hand.play_limit] cards. *)
