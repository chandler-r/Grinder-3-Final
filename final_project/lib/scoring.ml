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

(** The levels of all current hands. *)
let hand_levels = Hashtbl.create 12

let () =
  Hashtbl.add hand_levels "high card" 1;
  Hashtbl.add hand_levels "pair" 1;
  Hashtbl.add hand_levels "two pair" 1;
  Hashtbl.add hand_levels "three of a kind" 1;
  Hashtbl.add hand_levels "straight" 1;
  Hashtbl.add hand_levels "flush" 1;
  Hashtbl.add hand_levels "full house" 1;
  Hashtbl.add hand_levels "four of a kind" 1;
  Hashtbl.add hand_levels "straight flush" 1;
  Hashtbl.add hand_levels "five of a kind" 1;
  Hashtbl.add hand_levels "flush house" 1;
  Hashtbl.add hand_levels "flush five" 1

(** [match_hand_to_level_up_bonus hand] returns the tuple (c,m) corresponding to
    a hand type, where c and m are how many chips/what multiplier is added to
    the base value when the hand levels up. *)
let match_hand_to_level_up_bonus (hand : string) =
  match hand with
  | "high card" -> (10, 1.)
  | "pair" -> (15, 1.)
  | "two pair" -> (20, 1.)
  | "three of a kind" -> (20, 2.)
  | "straight" -> (30, 2.)
  | "flush" -> (15, 2.)
  | "full house" -> (25, 2.)
  | "four of a kind" -> (30, 3.)
  | "straight flush" -> (40, 3.)
  | "five of a kind" -> (35, 3.)
  | "flush house" -> (40, 3.)
  | "flush five" -> (40, 3.)
  | _ -> failwith "Unknown hand type."

(** [level_up_hand hand] modifies the hashtable values for the hand, [hand].
    Hand level goes up by 1 and the chips and mult update accordingly. *)
let level_up_hand (hand : Hand.hands) =
  let hand_type = Hand.played_hand_type hand in
  let bonus_chips, bonus_mult = match_hand_to_level_up_bonus hand_type in

  let prev_level = Hashtbl.find hand_levels hand_type in
  Hashtbl.replace hand_levels hand_type (prev_level + 1);

  let prev_chips = Hashtbl.find hand_base_chip_values hand_type in
  Hashtbl.replace hand_base_chip_values hand_type (prev_chips + bonus_chips);

  let prev_mult = Hashtbl.find hand_base_mult_values hand_type in
  Hashtbl.replace hand_base_mult_values hand_type (prev_mult +. bonus_mult);
  Printf.printf "Leveled up %s to level %d!\n" hand_type (prev_level + 1)

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

let calculate_card_contribution_acc jokers (chips_acc, mult_acc) card =
  let chips =
    chips_acc + Hashtbl.find card_base_chip_values (Card.number card)
  in
  let mult = mult_acc in
  print_endline
    ("Scored " ^ Card.to_string card ^ " : " ^ string_of_int chips ^ " x "
   ^ string_of_float mult);
  let new_chips, new_mult =
    Joker.apply_scoring_jokers_to_card jokers card chips mult
  in
  (new_chips, new_mult)

let score_played_cards played jokers =
  if played = [] then failwith "Cannot score empty hand."
  else
    let scored_hand_data = Hand.highest_hand played in
    let scored_hand_cards = snd scored_hand_data in
    let scored_hand_type = fst scored_hand_data |> Hand.played_hand_type in
    let base_chips = Hashtbl.find hand_base_chip_values scored_hand_type in
    let base_mult = Hashtbl.find hand_base_mult_values scored_hand_type in
    print_endline
      ("Scored hand type: " ^ String.capitalize_ascii scored_hand_type);
    print_endline
      ("Base : " ^ string_of_int base_chips ^ " x " ^ string_of_float base_mult);
    let curr_chips, curr_mult =
      List.fold_left
        (calculate_card_contribution_acc jokers)
        (base_chips, base_mult) scored_hand_cards
    in
    let total_chips, total_mult =
      Joker.apply_scoring_jokers_to_hand jokers played curr_chips curr_mult
    in
    let result =
      float_of_int total_chips *. total_mult |> ceil |> int_of_float
    in
    print_endline
      ("Total : " ^ string_of_int total_chips ^ " x "
     ^ string_of_float total_mult ^ " = " ^ string_of_int result);
    result
