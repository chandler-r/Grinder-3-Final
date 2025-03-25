type t
(* hand.t is the type for representing hands, i.e. some collection of cards. *)

type hands
(* represents the different types of hand that can be played. *)

exception TooManyCards
(* raised if the player tries to play a hand with more cards than what is
   allowed. *)

(*Functionality:

  check highest-scoring version of a collection of cards*)

val hand_size : int
(** [hand_size] is the maximum number of cards a player can have in their hand.
*)

val play_limit : int
(** [play_limit] is the number of cards that a player can play at a time. *)

val hand_limit : int
(** [hand_limit] is the maximum number of hands that can be played in a given
    round of the game*)

val highest_hand : t -> hands
(** [highest_score hand] is the highest score possible from a given set of
    cards. Final implementation should output a tuple int * hand_type.t or just
    a hand type, i.e. say whether the highest *)

val discard : t -> t -> t
(** [discard cards hand] discards a subset of cards in a hand. Should check that
    the number of allowed discards is >0 (if we're allowing discards) and
    replace them with cards from the deck, if available. *)

val play : t -> t -> t
(** [play cards hand] plays cards from the hand and replaces them with cards
    from the deck, if available. [highest_hand] is a helper function for it. *)
