open Card

type t

exception NotInDeck
(** Exception for when an operation cannot finish because the specified card is
    not in the deck. *)

val init : unit -> t
(** [init ()] initializes a 52 card deck. Different deck types can have
    different makeups of cards *)

val add_card : t -> Card.t -> t
(** [add_card c] adds the card c into the deck. This means decks are allowed to
    go above 52 cards. *)

val add_modifier : t -> Card.t -> t
(** TODO: Wait till later, might have to modify the card module in order to
    accomodate this, stretch goal. *)

val remove_card : t -> Card.t -> t
(** [remove card c] removes c from the deck. The card has to be in the deck,
    raises NotInDeck exception if not. This means decks are allowed to go under
    52 cards. *)

val draw_cards : t -> int -> Card.t list
(** [draw_cards n] draws n cards and returns them in a list. The cards are drawn
    at random. *)
