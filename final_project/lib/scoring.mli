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
    earned from the hand by the multiplier earned from the hand. As each card is
    scored, a running total of the chips and the multiplier is printed, along
    with the card that is being scored. The number of chips is an integer, the
    multiplier is a float, and the score is an integer. [played] must contain at
    least 1 card and no more than [Hand.play_limit] cards. *)
