type t
(** [t] represents a card in a standard 52-card deck (no jokers). The card will
    have a suit which is one of Spades, Hearts, Diamonds, or Clubs. The rank of
    the card will range from 2–10, Jack, Queen, King, or Ace. *)

type suit =
  | Spades
  | Hearts
  | Diamonds
  | Clubs
      (** [suit] represents the suit of the card, of which could be Spades,
          Hearts, Diamonds, or Clubs. *)

exception BadCard
(** [BadCard] is raised when a card is not a part of a standard deck. *)

val create_card : suit -> int -> t
(** [create_card s n] creates a card with suit [s] and rank [n]. Raises
    [BadCard] if [n] is not in the range of 1 to 14. *)

val of_pair : string * int -> t
(** [of_pair (s, n)] is a card with suite [s] and rank [n]. Raises
    [Invalid_argument] if such a card would not be in a standard 52-card deck.
*)

val number : t -> int
(** [number c] is the rank of [c], converted to an integer. *)

val suit : t -> string
(** [suit c] is the suit of [c]. One of Spades, Hearts, Diamonds, or Clubs. *)

val equal : t -> t -> bool
(** [equal c1 c2] is whether or not [c1] and [c2] have both the same suit and
    the same rank. *)

val to_string : t -> string
(** [to_string c] is a string representation of [c]. *)
