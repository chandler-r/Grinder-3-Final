type suit =
  | Spades
  | Hearts
  | Diamonds
  | Clubs

type t = suit * int
(** AF: [(Suit, n)] represents a card with rank [n] of suit [Suit]. Two through
    nine are self-describing rank. 11 is Jack, 12 is Queen, 13 is King, 1 and 14
    are both Ace. RI: [n] must be between 1 and 14, inclusive. *)

exception BadCard

let create_card s n : t = if n < 1 || n > 14 then raise BadCard else (s, n)

(** Requires [str] to be one of "spades", "hearts", "diamonds", or "clubs".
    Requires [n] to be between 1 and 14 (inclusive). *)
let of_pair (str, n) =
  if n < 1 || n > 14 then
    invalid_arg "Rank must be between 1 and 14 (inclusive)."
  else
    match String.lowercase_ascii str with
    | "spades" -> (Spades, n)
    | "hearts" -> (Hearts, n)
    | "diamonds" -> (Diamonds, n)
    | "clubs" -> (Clubs, n)
    | _ ->
        invalid_arg
          "Suit must be one of \"Spades\", \"Hearts\", \"Diamonds\", or \
           \"Clubs\"."

(** A card representing an Ace always gets sent to 14. *)
let number (s, n) = if n = 1 then 14 else n

(* Helper function that converts something of type [suit] to a string. *)
let string_of_suit s =
  match s with
  | Spades -> "♠️"
  | Hearts -> "❤️"
  | Diamonds -> "♦️"
  | Clubs -> "♣️"

(* Helper function that converts the number of a card to its rank. *)
let string_of_number n =
  match n with
  | 1 | 14 -> "Ace"
  | 2 -> "Two"
  | 3 -> "Three"
  | 4 -> "Four"
  | 5 -> "Five"
  | 6 -> "Six"
  | 7 -> "Seven"
  | 8 -> "Eight"
  | 9 -> "Nine"
  | 10 -> "Ten"
  | 11 -> "Jack"
  | 12 -> "Queen"
  | 13 -> "King"
  | _ -> invalid_arg "bad int"

let suit (s, n) = string_of_suit s
let equal (s1, n1) (s2, n2) = n1 = n2 && string_of_suit s1 = string_of_suit s2

(* of the form "<Rank> of <Suit>". The rank and suit are both capitalized.
   Example: "Five of Spades" *)
let to_string (s, n) = string_of_number n ^ " of " ^ string_of_suit s
