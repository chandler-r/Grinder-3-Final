type t
(* hand.t is the type for representing hands, i.e. some collection of cards. *)

type hands
(* represents the different types of hand that can be played. *)

exception TooManyCards
(* raised if the player tries to play a hand with more cards than what is
   allowed. *)

(*Functionality:

  check highest-scoring version of a collection of cards*)

val hand_size : int ref
(** [hand_size] is the maximum number of cards a player can have in their hand.
*)

val play_limit : int ref
(** [play_limit] is the number of cards that a player can play at a time. *)

val hands_per_round : int ref
(** [hands_per_round] is the maximum number of hands that can be played in a
    given round of the game*)

val highest_hand : t -> hands * t
(** [highest_hand hand] is the highest hand type possible from a given set of
    cards, along with the cards that will be used for scoring. *)

val discard : t -> t -> t
(** [discard cards hand] discards a subset of cards in a hand. Should check that
    the number of allowed discards is >0 (if we're allowing discards) and
    replace them with cards from the deck, if available. *)

val play : t -> t -> t * (hands * t)
(** [play cards hand] plays cards from the hand and replaces them with cards
    from the deck, if available. [highest_hand] is a helper function for it.
    Returns the new hand, the highest possible corresponding hand type of the
    played cards, and the played cards themselves for scoring. *)

val to_list : t -> Card.t list
(** [to_list cards] returns a list of cards in hand. *)
