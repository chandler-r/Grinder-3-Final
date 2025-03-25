type t
(* represents a card *)

exception BadCard

val of_pair : string * int -> t
(* (spades, 5) --> 5 of spades *)

val number : t -> int
(* eg 5 of spades --> 5 *)

val suit : t -> string
(* eg 5 of spades --> spades *)

val to_string : t -> string
(* eg "5 of spades" *)
