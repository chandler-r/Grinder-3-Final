val money : unit -> int ref
(** [money ()] initializes the mutable amount of money that the player has
    throughout the game. It starts at $4 for a new game. *)

exception InsufficientFunds
(** Raised when trying to pay more money than is currently owned. *)

val pay : int -> int ref -> unit
(** [pay x money] reduces [!money] by an amount [x]. Raises [InsufficientFunds]
    if [x > !money]. *)

val end_of_round : Level.level -> int -> int ref -> string
(** [end_of_round blind hands money] adjusts [!money] for the end of the round.
    Small, big, and boss blinds give $3, $4, and $5 respectively. Players earn
    up to $5 of interest based on how much money they have in hand at the end of
    the round and earn $1 for each remaining hand at the end of the round.
    Returns a string of all sources of income for the round. *)
