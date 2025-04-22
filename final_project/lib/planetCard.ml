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
  if Hand.played_hand_type hand = "pair" then (mult +. 1., chips + 15)
  else (mult, chips)

let use_venus hand mult chips =
  if Hand.played_hand_type hand = "three of a kind" then (mult +. 2., chips + 20)
  else (mult, chips)

let use_earth hand mult chips =
  if Hand.played_hand_type hand = "full house" then (mult +. 2., chips + 25)
  else (mult, chips)

let use_mars hand mult chips =
  if Hand.played_hand_type hand = "four of a kind" then (mult +. 3., chips + 30)
  else (mult, chips)

let use_jupiter hand mult chips =
  if Hand.played_hand_type hand = "flush" then (mult +. 2., chips + 15)
  else (mult, chips)

let use_saturn hand mult chips =
  if Hand.played_hand_type hand = "straight" then (mult +. 3., chips + 30)
  else (mult, chips)

let use_uranus hand mult chips =
  if Hand.played_hand_type hand = "two pair" then (mult +. 1., chips + 20)
  else (mult, chips)

let use_neptune hand mult chips =
  if Hand.played_hand_type hand = "straight flush" then (mult +. 4., chips + 40)
  else (mult, chips)

let use_pluto hand mult chips =
  if Hand.played_hand_type hand = "high card" then (mult +. 1., chips + 10)
  else (mult, chips)

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
