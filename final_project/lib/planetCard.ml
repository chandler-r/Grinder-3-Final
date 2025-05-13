type t =
  | Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune
  | Pluto
(* AF: [Mercury] is the planet card that levels up pairs. [Venus] is the planet
   card that levels up three of a kinds. [Earth] is the planet card that levels
   up full houses. [Mars] is the planet card that levels up four of a kinds.
   [Jupiter] is the planet card that levels up flushes. [Saturn] is the planet
   card that levels up straights. [Uranus] is the planet card that levels up two
   pairs. [Neptune] is the planet card that levels up straight flushes. [Pluto]
   is the planet card that levels up high cards.*)

let to_string = function
  | Mercury -> "Mercury"
  | Venus -> "Venus"
  | Earth -> "Earth"
  | Mars -> "Mars"
  | Jupiter -> "Jupiter"
  | Saturn -> "Saturn"
  | Uranus -> "Uranus"
  | Neptune -> "Neptune"
  | Pluto -> "Pluto"

let of_hand = function
  | "pair" -> Mercury
  | "three of a kind" -> Venus
  | "full house" -> Earth
  | "four of a kind" -> Mars
  | "flush" -> Jupiter
  | "straight" -> Saturn
  | "two pair" -> Uranus
  | "straight flush" -> Neptune
  | "high card" -> Pluto
  | _ -> failwith "This hand is currently unsupported."

let to_hand = function
  | Mercury -> "Pair"
  | Venus -> "Three of a Kind"
  | Earth -> "Full House"
  | Mars -> "Four of a Kind"
  | Jupiter -> "Flush"
  | Saturn -> "Straight"
  | Uranus -> "Two Pair"
  | Neptune -> "Straight Flush"
  | Pluto -> "High Card"

let of_string = function
  | "Mercury" -> Mercury
  | "Venus" -> Venus
  | "Earth" -> Earth
  | "Mars" -> Mars
  | "Jupiter" -> Jupiter
  | "Saturn" -> Saturn
  | "Uranus" -> Uranus
  | "Neptune" -> Neptune
  | "Pluto" -> Pluto
  | _ ->
      failwith
        "Only Mercury through Pluto planet cards are supported currently."

let use_mercury hand mult chips =
  if Hand.played_hand_type hand = "pair" then (
    mult := !mult +. 1.;
    chips := !chips + 15)

let use_venus hand mult chips =
  if Hand.played_hand_type hand = "three of a kind" then (
    mult := !mult +. 2.;
    chips := !chips + 20)

let use_earth hand mult chips =
  if Hand.played_hand_type hand = "full house" then (
    mult := !mult +. 2.;
    chips := !chips + 25)

let use_mars hand mult chips =
  if Hand.played_hand_type hand = "four of a kind" then (
    mult := !mult +. 3.;
    chips := !chips + 30)

let use_jupiter hand mult chips =
  if Hand.played_hand_type hand = "flush" then (
    mult := !mult +. 2.;
    chips := !chips + 15)

let use_saturn hand mult chips =
  if Hand.played_hand_type hand = "straight" then (
    mult := !mult +. 3.;
    chips := !chips + 30)

let use_uranus hand mult chips =
  if Hand.played_hand_type hand = "two pair" then (
    mult := !mult +. 1.;
    chips := !chips + 20)

let use_neptune hand mult chips =
  if Hand.played_hand_type hand = "straight flush" then (
    mult := !mult +. 4.;
    chips := !chips + 40)

let use_pluto hand mult chips =
  if Hand.played_hand_type hand = "high card" then (
    mult := !mult +. 1.;
    chips := !chips + 10)

(* https://balatrogame.fandom.com/wiki/Planet_Cards *)
let use_planet_card card hand mult chips =
  match card with
  | Mercury -> use_mercury hand mult chips
  | Venus -> use_venus hand mult chips
  | Earth -> use_earth hand mult chips
  | Mars -> use_mars hand mult chips
  | Jupiter -> use_jupiter hand mult chips
  | Saturn -> use_saturn hand mult chips
  | Uranus -> use_uranus hand mult chips
  | Neptune -> use_neptune hand mult chips
  | Pluto -> use_pluto hand mult chips
