type t (* = some list or array of cards *)
type hands = HighCard

exception TooManyCards

let hand_size = 7
let play_limit = 5
let hand_limit = 3
let highest_hand x = HighCard

(*TODO*)
(* will need some pattern match on the number/value corresponding to each card
   in the hand *)
let discard cards hand = hand (*TODO*)
let play cards hand = hand (*TODO*)
