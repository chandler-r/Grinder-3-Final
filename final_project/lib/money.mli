val money : int ref
(** The mutable amount of money that the player has throughout the game *)

exception InsufficientFunds

(* type blind *)
(** Will not be in the final implementation, but it's needed to calculate
    end-of-round payouts and I'm not sure where it will be defined. *)

val pay : int -> unit
(** [pay x] reduces [money] by an amount [x]. Fails if [x > !money]. *)

val end_of_round : Level.level -> int -> unit
(** [end_of_round blind hands] adjusts money for the end of the round. Small,
    big, and boss blinds give $3, $4, and $5 respectively. Players earn up to $5
    of interest based on how much money they have in hand at the end of the
    round and earn $1 for each remaining hand at the end of the round. *)
