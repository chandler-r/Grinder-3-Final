type t
(** The type of a joker. Effects taken from
    https://balatrogame.fandom.com/wiki/Jokers. *)

val joker_limit : int ref
(** The maximum number of jokers that can be in play at one time. *)

exception TooManyJokers
(** Raised when trying to add a joker to a list that already contains
    [joker_limit] jokers *)

val to_string : t -> string
(** [to_string joker] is the string representing the name of a specific kind of
    joker. *)

val add_joker : t array -> t -> t array
(** [add_joker jokers j] is the array containing joker [j] as well as all the
    jokers in [jokers] if [jokers] contains less than [joker_limit] jokers.
    Raises [TooManyJokers] if [jokers] contains [joker_limit] jokers or more. *)

val apply_scoring_jokers_to_card :
  t array -> Card.t -> int -> float -> int * float
(** [apply_scoring_jokers_to_card jokers card chips mult] is a pair whose first
    element is the new number of chips and whose second number is the new mult
    after applying the effects of all jokers in [jokers] to played card [card].
    Each time a joker is applied, prints the name of that joker, as well as the
    new chips and mult values. *)

val apply_scoring_jokers_to_hand :
  t array -> Card.t list -> int -> float -> int * float
(** [apply_scoring_jokers_to_hand jokers hand chips mult] is a pair whose first
    element is the new number of chips and whose second number is the new mult
    after applying the effects of all jokers in [jokers] to played hand [hand].
    Each time a joker is applied, prints the name of that joker, as well as the
    new chips and mult values. Requires that all the individual cards in [hand]
    have already been scored. *)

val of_string : string -> t
(** [joker_of_string_pair s] is a joker of kind [s]. NOTE: In current
    implementation, [s] must be one of
    [["Joker"; "Misprint"; "GreedyJoker"; "LustyJoker"; "WrathfulJoker";
     "GluttonousJoker"; "JollyJoker"; "ZanyJoker"; "MadJoker"; "CrazyJoker";
     "DrollJoker"; "SlyJoker"; "WilyJoker"; "CleverJoker"; "DeviousJoker";
     "CraftyJoker"; "HalfJoker"; "Fibonacci"; "EvenSteven"; "OddTodd";
     "Scholar"; "Bloodstone"; "Arrowhead"; "OnyxAgate"; "TheDuo"; "TheTrio";
     "TheFamily"; "TheOrder"; "TheTribe"; "Triboulet"]]*)

val rarity : t -> string
(** [rarity joker] is one of ["Common"], ["Uncommon"], ["Rare"], and
    ["Legendary"]. *)

(* stuff from before:  *)
(* type joker = | Joker (* +4 Mult *) | GreedyJoker (* Played cards with Diamond
   suit give +3 Mult when scored *) | LustyJoker (* Played cards with Heart suit
   give +3 Mult when scored *) | WrathfulJoker (* Played cards with Spade suit
   give +3 Mult when scored *) | GluttonousJoker (* Played cards with Club suit
   give +3 Mult when scored *) | JollyJoker (* +8 Mult if played hand contains a
   Pair *) | ZanyJoker (* +12 Mult if played hand contains a Three of a Kind *)
   | MadJoker (* +10 Mult if played hand contains a Two Pair *) | CrazyJoker (*
   +12 Mult if played hand contains a Straight *) | DrollJoker (* +10 Mult if
   played hand contains a Flush *) | SlyJoker (* +50 Chips if played hand
   contains a Pair *) | WilyJoker (* +100 Chips if played hand contains a Three
   of a Kind *) | CleverJoker (* +80 Chips if played hand contains a Two Pair *)
   | DeviousJoker (* +100 Chips if played hand contains a Straight *) |
   CraftyJoker (* +80 Chips if played hand contains a Flush *) | HalfJoker (*
   +20 Mult if played hand contains 3 or fewer cards *) | Fibonacci (* Each
   played Ace, 2, 3, 5, or 8 gives +8 Mult when scored *) | EvenSteven (* Played
   cards with even rank give +4 Mult when scored (10, 8, 6, 4, 2) *) | OddTodd
   (* Played cards with odd rank give +31 Chips when scored (A, 9, 7, 5, 3) *) |
   Scholar (* Played Aces give +20 Chips and +4 Mult when scored *) | Bloodstone
   (* 1 in 2 chance for played cards with Heart suit to give X1.5 Mult when
   scored *) | Arrowhead (* Played cards with Spade suit give +50 Chips when
   scored *) | OnyxAgate (* Played cards with Club suit give +7 Mult when scored
   *) | TheDuo (* X2 Mult if played hand contains a Pair *) | TheTrio (* X3 Mult
   if played hand contains a Three of a Kind *) | TheFamily (* X4 Mult if played
   hand contains a Four of a Kind *) | TheOrder (* X3 Mult if played hand
   contains a Straight *) | TheTribe (* X2 Mult if played hand contains a Flush
   *) | Triboulet (* Played Kings and Queens each give X2 Mult when scored *) *)
