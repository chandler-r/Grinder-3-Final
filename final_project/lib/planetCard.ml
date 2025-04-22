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
