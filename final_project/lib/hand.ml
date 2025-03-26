type t = Card.t list
(* AF: [card1; card2; ...; cardn] is a representation of a collection of cards, either held in hand or a subset of those to be played or discarded. *)
(* RI: the length of the list representing a hand should be no more than hand_size. The length of the list representing cards played or discarded should be no more than play_limit *)

type hands =
  | HighCard
  | Pair
  | TwoPair
  | ThreeKind
  | Straight
  | Flush
  | FullHouse
  | FourKind
  | StraightFlush
  | RoyalFlush

exception TooManyCards

let hand_size = ref 7
let play_limit = ref 5
let hands_per_round = ref 3

let rep_ok_hand hand =
  if List.length hand <= !hand_size then hand else raise TooManyCards

let rep_ok_play play =
  if List.length play <= !play_limit then play else raise TooManyCards

let highest_hand play = (HighCard, play)
(*TODO*)
(* will need some pattern match on the number/value corresponding to each card
   in the hand *)

let cycle_cards cards hand =
  let len = List.length cards in
  List.filter (Fun.negate (fun y -> List.mem y cards)) hand
(*@ Deck.draw len*)
(*TODO: will need to use some Deck.draw function where we draw [len] cards if
  possible*)

let discard cards hand =
  let x = rep_ok_hand cards in
  cycle_cards x hand

let play cards hand =
  let x = rep_ok_play cards in
  (cycle_cards x hand, highest_hand cards)

let to_list cards = cards
