type t
(** Type representing shop items. *)

val open_shop : int ref -> Deck.t ref -> Joker.t array ref -> unit
(** [open_shop money deck jokers] opens a shop that allows you to buy cards,
    jokers, and card modifiers with [money]. [deck] and [joker] are global
    variables that are modified when buying items. *)

val process_purchase : t -> Deck.t ref -> Joker.t array ref -> unit
(** [process_purchase purchase deck jokers] processes a purchase, [purchase]
    you've made. This function is meant to be used in a map to process a list of
    purchases. The purchase is passed on to its respective process function to
    change any global variables necessary. *)
