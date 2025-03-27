type t = int
type chips = int
type mult = float

(* AF: [x : t] is the total score earned by a player in the current round. [y : chips] is the number of chips a player has earned from playing a hand. [z : mult] is the score multiplier a player has earned from playing a hand. *)
(* RI: The integers representing the score and the number of chips must be nonnegative. The float representing the multiplier must be greater than or equal to 1.0. *)

let get_score score = score
let get_chips chips = chips
let get_mult mult = mult

exception InvalidScoring

let rep_ok_score score = if score >= 0 then score else raise InvalidScoring
let rep_ok_chips chips = if chips >= 0 then chips else raise InvalidScoring
let rep_ok_mult mult = if mult >= 1.0 then mult else raise InvalidScoring

(* The following definition was inspired by the example from
   https://ocaml.org/manual/5.3/api/Hashtbl.html*)

(** Maps each rank ([Card.number card] for some [card: Card.t]) to a chip value:
    Number cards are worth their number in chips, face cards are worth 10 chips,
    and Aces are worth 11 chips. *)
let card_base_chip_values =
  Seq.ints 2 |> Seq.take 14 (* 2...14 *)
  |> Seq.map (fun (x : int) ->
         if x > 10 && x < 14 then (x, 10) else if x = 14 then (x, 11) else (x, x))
  |> Hashtbl.of_seq

(** Maps a played hand ([Hand.highest_hand played |> fst] for some
    [played: Card.t list]) to a chip value. *)
let hand_base_chip_values = Hashtbl.create 12

let () =
  Hashtbl.add hand_base_chip_values "high card" 5;
  Hashtbl.add hand_base_chip_values "pair" 10;
  Hashtbl.add hand_base_chip_values "two pair" 20;
  Hashtbl.add hand_base_chip_values "three of a kind" 30;
  Hashtbl.add hand_base_chip_values "straight" 30;
  Hashtbl.add hand_base_chip_values "flush" 35;
  Hashtbl.add hand_base_chip_values "full house" 40;
  Hashtbl.add hand_base_chip_values "four of a kind" 60;
  Hashtbl.add hand_base_chip_values "straight flush" 100;
  Hashtbl.add hand_base_chip_values "five of a kind" 120;
  Hashtbl.add hand_base_chip_values "flush house" 140;
  Hashtbl.add hand_base_chip_values "flush five" 160

(** Maps a played hand ([Hand.highest_hand played |> fst] for some
    [played: Card.t list]) to a multiplier. *)
let hand_base_mult_values = Hashtbl.create 12

let () =
  Hashtbl.add hand_base_mult_values "high card" 1.;
  Hashtbl.add hand_base_mult_values "pair" 2.;
  Hashtbl.add hand_base_mult_values "two pair" 2.;
  Hashtbl.add hand_base_mult_values "three of a kind" 3.;
  Hashtbl.add hand_base_mult_values "straight" 4.;
  Hashtbl.add hand_base_mult_values "flush" 4.;
  Hashtbl.add hand_base_mult_values "full house" 4.;
  Hashtbl.add hand_base_mult_values "four of a kind" 7.;
  Hashtbl.add hand_base_mult_values "straight flush" 8.;
  Hashtbl.add hand_base_mult_values "five of a kind" 12.;
  Hashtbl.add hand_base_mult_values "flush house" 14.;
  Hashtbl.add hand_base_mult_values "flush five" 16.

(** Helper function to visualize hands in system output. *)
let card_list_printer cards =
  if List.length cards = 0 then "None"
  else
    List.fold_left
      (fun str card -> str ^ " " ^ Card.to_string card ^ ";")
      "" cards
    |> fun x -> "[" ^ String.sub x 0 (String.length x - 1) ^ " ]"

let calculate_chips played =
  let scored_hand_data = Hand.highest_hand played in
  let scored_hand_type = fst scored_hand_data |> Hand.played_hand_type in
  let scored_cards = snd scored_hand_data in
  List.fold_left
    (fun acc card ->
      acc + Hashtbl.find card_base_chip_values (Card.number card))
    0 scored_cards
  + Hashtbl.find hand_base_chip_values scored_hand_type

let calculate_mult played =
  let scored_hand_data = Hand.highest_hand played in
  let scored_hand_type = fst scored_hand_data |> Hand.played_hand_type in
  Hashtbl.find hand_base_mult_values scored_hand_type

let score_played_cards played =
  float_of_int (rep_ok_chips (calculate_chips played))
  *. rep_ok_mult (calculate_mult played)
  |> ceil |> int_of_float
