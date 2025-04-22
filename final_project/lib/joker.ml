let () = Random.self_init ()

type rarity =
  | Common
  | Uncommon
  | Rare
  | Legendary

type kind =
  | Joker (* +4 Mult *)
  | GreedyJoker (* Played cards with Diamond suit give +3 Mult when scored *)
  | LustyJoker (* Played cards with Heart suit give +3 Mult when scored *)
  | WrathfulJoker (* Played cards with Spade suit give +3 Mult when scored *)
  | GluttonousJoker (* Played cards with Club suit give +3 Mult when scored *)
  | JollyJoker (* +8 Mult if played hand contains a Pair *)
  | ZanyJoker (* +12 Mult if played hand contains a Three of a Kind *)
  | MadJoker (* +10 Mult if played hand contains a Two Pair *)
  | CrazyJoker (* +12 Mult if played hand contains a Straight *)
  | DrollJoker (* +10 Mult if played hand contains a Flush *)
  | SlyJoker (* +50 Chips if played hand contains a Pair *)
  | WilyJoker (* +100 Chips if played hand contains a Three of a Kind *)
  | CleverJoker (* +80 Chips if played hand contains a Two Pair *)
  | DeviousJoker (* +100 Chips if played hand contains a Straight *)
  | CraftyJoker (* +80 Chips if played hand contains a Flush *)
  | HalfJoker (* +20 Mult if played hand contains 3 or fewer cards *)
  | Fibonacci (* Each played Ace, 2, 3, 5, or 8 gives +8 Mult when scored *)
  | EvenSteven
    (* Played cards with even rank give +4 Mult when scored (10, 8, 6, 4, 2) *)
  | OddTodd
    (* Played cards with odd rank give +31 Chips when scored (A, 9, 7, 5, 3) *)
  | Scholar (* Played Aces give +20 Chips and +4 Mult when scored *)
  | Bloodstone
    (* 1 in 2 chance for played cards with Heart suit to give X1.5 Mult when
       scored *)
  | Arrowhead (* Played cards with Spade suit give +50 Chips when scored *)
  | OnyxAgate (* Played cards with Club suit give +7 Mult when scored *)
  | TheDuo (* X2 Mult if played hand contains a Pair *)
  | TheTrio (* X3 Mult if played hand contains a Three of a Kind *)
  | TheFamily (* X4 Mult if played hand contains a Four of a Kind *)
  | TheOrder (* X3 Mult if played hand contains a Straight *)
  | TheTribe (* X2 Mult if played hand contains a Flush *)
  | Triboulet (* Played Kings and Queens each give X2 Mult when scored *)
      (** The type of a joker. Effects taken from
          https://balatrogame.fandom.com/wiki/Jokers. *)

type t = kind * rarity

let joker_limit = ref 5

exception TooManyJokers

let kind_to_string = function
  | Joker -> "Joker"
  | GreedyJoker -> "Greedy Joker"
  | LustyJoker -> "Lusty Joker"
  | WrathfulJoker -> "Wrathful Joker"
  | GluttonousJoker -> "Gluttonous Joker"
  | JollyJoker -> "Jolly Joker"
  | ZanyJoker -> "Zany Joker"
  | MadJoker -> "Mad Joker"
  | CrazyJoker -> "Crazy Joker"
  | DrollJoker -> "Droll Joker"
  | SlyJoker -> "Sly Joker"
  | WilyJoker -> "Wily Joker"
  | CleverJoker -> "Clever Joker"
  | DeviousJoker -> "Devious Joker"
  | CraftyJoker -> "Crafty Joker"
  | HalfJoker -> "Half Joker"
  | Fibonacci -> "Fibonacci"
  | EvenSteven -> "Even Steven"
  | OddTodd -> "Odd Todd"
  | Scholar -> "Scholar"
  | Bloodstone -> "Bloodstone"
  | Arrowhead -> "Arrowhead"
  | OnyxAgate -> "Onyx Agate"
  | TheDuo -> "The Duo"
  | TheTrio -> "The Trio"
  | TheFamily -> "The Family"
  | TheOrder -> "The Order"
  | TheTribe -> "The Tribe"
  | Triboulet -> "Triboulet"

let rarity_to_string = function
  | Common -> "Common"
  | Uncommon -> "Uncommon"
  | Rare -> "Rare"
  | Legendary -> "Legendary"

let to_string (j, r) = kind_to_string j ^ " (" ^ rarity_to_string r ^ ")"

let add_joker joker_arr joker =
  if Array.length joker_arr >= !joker_limit then raise TooManyJokers
  else Array.append joker_arr [| joker |]

(* Generate a random integer between 1 and 2. *)
let roll_probability_half () = 1 + Random.int 2
let apply_basic mult j_app = mult := !mult +. 3.

let apply_greedy suit mult j_app =
  if suit = "♦️" then mult := !mult +. 3. else j_app := false

let apply_lusty suit mult j_app =
  if suit = "❤️" then mult := !mult +. 3. else j_app := false

let apply_wrathful suit mult j_app =
  if suit = "♠️" then mult := !mult +. 3. else j_app := false

let apply_gluttonous suit mult j_app =
  if suit = "♣️" then mult := !mult +. 3. else j_app := false

let apply_jolly (suit1, suit2) rank mult j_app = failwith "unimplemented"
let apply_zany j_app = failwith "unimplemented"
let apply_mad j_app = failwith "unimplemented"
let apply_crazy j_app = failwith "unimplemented"
let apply_droll j_app = failwith "unimplemented"
let apply_sly j_app = failwith "unimplemented"
let apply_wily j_app = failwith "unimplemented"
let apply_clever j_app = failwith "unimplemented"
let apply_devious j_app = failwith "unimplemented"
let apply_crafty j_app = failwith "unimplemented"
let apply_half j_app = failwith "unimplemented"

let apply_fib rank mult j_app =
  match rank with
  | 14 | 2 | 3 | 5 | 8 -> mult := !mult +. 8.
  | _ -> j_app := false

let apply_even rank mult j_app =
  match rank with
  | 2 | 4 | 6 | 8 | 10 -> mult := !mult +. 4.
  | _ -> j_app := false

let apply_odd rank chips j_app =
  match rank with
  | 14 | 3 | 5 | 7 | 9 -> chips := !chips + 31
  | _ -> j_app := false

let apply_scholar rank chips mult j_app =
  match rank with
  | 14 ->
      chips := !chips + 20;
      mult := !mult +. 4.
  | _ -> j_app := false

let apply_blood suit mult j_app =
  if suit = "❤️" && roll_probability_half () = 2 then mult := !mult *. 1.5
  else j_app := false

let apply_arrow suit chips j_app =
  if suit = "♠️" then chips := !chips + 50 else j_app := false

let apply_onyx suit mult j_app =
  if suit = "♣️" then mult := !mult +. 7. else j_app := false

let apply_duo j_app = failwith "unimplemented"
let apply_trio j_app = failwith "unimplemented"
let apply_family j_app = failwith "unimplemented"
let apply_order j_app = failwith "unimplemented"
let apply_tribe j_app = failwith "unimplemented"

let apply_triboulet rank mult j_app =
  match rank with
  | 12 | 13 -> mult := !mult *. 2.
  | _ -> j_app := false

let apply_single_joker joker suit rank chips mult j_app =
  match joker with
  | Joker, _ -> apply_basic mult j_app
  | GreedyJoker, _ -> apply_greedy suit mult j_app
  | LustyJoker, _ -> apply_lusty suit mult j_app
  | WrathfulJoker, _ -> apply_wrathful suit mult j_app
  | GluttonousJoker, _ -> apply_gluttonous suit mult j_app
  | JollyJoker, _ -> apply_jolly (suit, suit) rank mult j_app
  | ZanyJoker, _ -> apply_zany j_app
  | MadJoker, _ -> apply_mad j_app
  | CrazyJoker, _ -> apply_crazy j_app
  | DrollJoker, _ -> apply_droll j_app
  | SlyJoker, _ -> apply_sly j_app
  | WilyJoker, _ -> apply_wily j_app
  | CleverJoker, _ -> apply_clever j_app
  | DeviousJoker, _ -> apply_devious j_app
  | CraftyJoker, _ -> apply_crafty j_app
  | HalfJoker, _ -> apply_half j_app
  | Fibonacci, _ -> apply_fib rank mult j_app
  | EvenSteven, _ -> apply_even rank mult j_app
  | OddTodd, _ -> apply_odd rank chips j_app
  | Scholar, _ -> apply_scholar rank chips mult j_app
  | Bloodstone, _ -> apply_blood suit mult j_app
  | Arrowhead, _ -> apply_arrow suit chips j_app
  | OnyxAgate, _ -> apply_onyx suit mult j_app
  | TheDuo, _ -> apply_duo j_app
  | TheTrio, _ -> apply_trio j_app
  | TheFamily, _ -> apply_family j_app
  | TheOrder, _ -> apply_order j_app
  | TheTribe, _ -> apply_tribe j_app
  | Triboulet, _ -> apply_triboulet rank mult j_app

let apply_scoring_jokers_to_card_helper (card : Card.t)
    (chips_and_mult : int * float) (joker : t) =
  let chips = ref (fst chips_and_mult) in
  let mult = ref (snd chips_and_mult) in
  let joker_applied = ref true in
  apply_single_joker joker (Card.suit card) (Card.number card) chips mult
    joker_applied;
  if !joker_applied then
    print_endline
      ("Applied " ^ to_string joker ^ ": " ^ string_of_int !chips ^ " x "
     ^ string_of_float !mult);
  (!chips, !mult)

let apply_scoring_jokers_to_card joker_arr card chips mult =
  Array.fold_left
    (apply_scoring_jokers_to_card_helper card)
    (chips, mult) joker_arr

let kind_of_string = function
  | "Joker" -> Joker
  | "GreedyJoker" -> GreedyJoker
  | "LustyJoker" -> LustyJoker
  | "WrathfulJoker" -> WrathfulJoker
  | "GluttonousJoker" -> GluttonousJoker
  | "JollyJoker" -> JollyJoker
  | "ZanyJoker" -> ZanyJoker
  | "MadJoker" -> MadJoker
  | "CrazyJoker" -> CrazyJoker
  | "DrollJoker" -> DrollJoker
  | "SlyJoker" -> SlyJoker
  | "WilyJoker" -> WilyJoker
  | "CleverJoker" -> CleverJoker
  | "DeviousJoker" -> DeviousJoker
  | "CraftyJoker" -> CraftyJoker
  | "HalfJoker" -> HalfJoker
  | "Fibonacci" -> Fibonacci
  | "EvenSteven" -> EvenSteven
  | "OddTodd" -> OddTodd
  | "Scholar" -> Scholar
  | "Bloodstone" -> Bloodstone
  | "Arrowhead" -> Arrowhead
  | "OnyxAgate" -> OnyxAgate
  | "TheDuo" -> TheDuo
  | "TheTrio" -> TheTrio
  | "TheFamily" -> TheFamily
  | "TheOrder" -> TheOrder
  | "TheTribe" -> TheTribe
  | "Triboulet" -> Triboulet
  | _ -> failwith "Not a kind of joker"

let rarity_of_string = function
  | "Common" -> Common
  | "Uncommon" -> Uncommon
  | "Rare" -> Rare
  | "Legendary" -> Legendary
  | _ -> failwith "Not a kind of rarity"

let of_string str = (kind_of_string str, Common)
let rarity (k, r) = rarity_to_string r
