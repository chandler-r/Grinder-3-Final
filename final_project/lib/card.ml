type suit =
  | Spades
  | Hearts
  | Diamonds
  | Clubs

type t = suit * int
(* eg [Spades, n] represents "n of spades". [n] must be 1 through 14 inclusive.
   1 and 14 represent ace. 11 represents jack. 12 represents queen. 13
   represents king. *)

exception BadCard

let of_pair (str, n) =
  if n < 1 || n > 14 then invalid_arg "bad int"
  else
    match str with
    | "Spades" -> (Spades, n)
    | "Hearts" -> (Hearts, n)
    | "Diamonds" -> (Diamonds, n)
    | "Clubs" -> (Clubs, n)
    | _ -> invalid_arg "bad suit"

let number (s, n) = n

let string_of_suit s =
  match s with
  | Spades -> "Spades"
  | Hearts -> "Hearts"
  | Diamonds -> "Diamonds"
  | Clubs -> "Clubs"

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
  | _ -> raise BadCard

let suit (s, n) = string_of_suit s
let to_string (s, n) = string_of_number n ^ " of " ^ string_of_suit s
