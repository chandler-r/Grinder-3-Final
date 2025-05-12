type t
(** The type of a planet card, used to upgrade hand levels (played hands give
    more chips and mult per level). *)

val to_string : t -> string
(** [to_string p] is a string representation of the planet card [p]. *)

val of_string : string -> t
(** [of_string s] is the planet card represented by the string [s]. [s] must be
    one of ["Mercury"], ["Venus"]. ["Earth"], ["Mars"], ["Jupiter"], ["Saturn"],
    ["Uranus"], ["Neptune"], or ["Pluto"]. *)

val to_hand : t -> string
(** [to_hand p] is a lowercase string representation of the hand upgraded by
    planet card [p]. *)

val of_hand : string -> t
(** [of_hand s] is the planet card that upgrades the hand represented by the
    string [s]. [s] must be one of ["pair"], ["three of a kind"].
    ["full house"], ["four of a kind"], ["flush"], ["straight"], ["two pair"],
    ["straight flush"], or ["high card"]. *)

(* NOTE: never used? *)
val use_planet_card : t -> Hand.hands -> float ref -> int ref -> unit
