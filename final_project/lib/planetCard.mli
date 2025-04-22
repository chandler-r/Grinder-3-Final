type t

val to_string : t -> string
val of_string : string -> t
val use_planet_card : t -> Hand.hands -> float -> int -> float * int
