type t =
  | DeckPurchase of Card.t
  | JokerPurchase of Joker.t
  | PlanetPurchase of Hand.hands

(* AF: [DeckPurchase (x)] represents a purchase of a card [x]. 
[JokerPurchase (y)] represents a purchase of a joker [y].
[Planet_card__purchase (z)] represents a purchase of a planet card for 
hand type [z]. *)
(* RI: [x] must be a valid card. [y] must be a valid joker. [z] must be 
a valid hand type. *)

(** [process_deck_purchase deck purchase] adds the card [purchase] into the deck
    [deck]. Modifies the global reference. *)
let process_deck_purchase (deck : Deck.t ref) (purchase : Card.t) =
  deck := Deck.add_card !deck purchase;
  print_endline "Card added to deck!"

(** [process_joker_purchase jokers purchase] adds the joker [purchase] into the
    array of jokers represented by [jokers]. If there are too many jokers in
    [jokers], will instead print a message to the user. *)
let process_joker_purchase (jokers : Joker.t array ref) (purchase : Joker.t) =
  try
    jokers := Joker.add_joker !jokers purchase;
    print_endline "Joker added!"
  with Joker.TooManyJokers -> print_endline "Cannot add more jokers."

(** [process_planet_card purchase] levels up the hand associated with
    [purchase].*)
let process_planet_card (purchase : Hand.hands) =
  Scoring.level_up_hand purchase;
  print_endline "Planet card applied!"

let process_purchase (purchase : t) (deck : Deck.t ref)
    (jokers : Joker.t array ref) =
  match purchase with
  | DeckPurchase purchase -> process_deck_purchase deck purchase
  | JokerPurchase purchase -> process_joker_purchase jokers purchase
  | PlanetPurchase purchase -> process_planet_card purchase

let card_purchase () =
  let rank = Random.int 13 + 1 in
  let suit =
    match Random.int 4 with
    | 0 -> "Spades"
    | 1 -> "Hearts"
    | 2 -> "Diamonds"
    | _ -> "Clubs"
  in
  Card.of_pair (suit, rank)

let load_file filename =
  let file = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line file in
      read_lines (line :: acc)
    with End_of_file ->
      close_in file;
      List.rev acc
  in
  read_lines []

let common_jokers = load_file "data/common_jokers.txt"
let uncommon_jokers = load_file "data/uncommon_jokers.txt"
let rare_jokers = load_file "data/rare_jokers.txt"
let legendary_jokers = load_file "data/legendary_jokers.txt"

(* let joker_purchase () = let joker_list = load_file "data/jokers.txt" in let
   rand_idx = Random.int (List.length joker_list) in let random_joker = List.nth
   joker_list rand_idx in Joker.of_string random_joker *)

(** Generates a random joker of a certain rarity. The rarity is determined by
    the integer [rarity]. [rarity] must always be between 1 and 100. *)
let rec generate_joker (jokers : Joker.t array ref) (rarity : int) =
  if rarity <= 70 then
    let random_joker =
      Joker.of_string
        (List.nth common_jokers (Random.int (List.length common_jokers)))
    in
    if Array.mem random_joker !jokers then
      generate_joker jokers (Random.int 100 + 1)
    else random_joker
  else if rarity <= 95 then
    let random_joker =
      Joker.of_string
        (List.nth uncommon_jokers (Random.int (List.length uncommon_jokers)))
    in
    if Array.mem random_joker !jokers then
      generate_joker jokers (Random.int 100 + 1)
    else random_joker
  else if rarity <= 99 then
    let random_joker =
      Joker.of_string
        (List.nth rare_jokers (Random.int (List.length rare_jokers)))
    in
    if Array.mem random_joker !jokers then
      generate_joker jokers (Random.int 100 + 1)
    else random_joker
  else
    let random_joker =
      Joker.of_string
        (List.nth legendary_jokers (Random.int (List.length legendary_jokers)))
    in
    if Array.mem random_joker !jokers then
      generate_joker jokers (Random.int 100 + 1)
    else random_joker

(** [joker_cost j] is the cost of joker [j], determined by its rarity. Common
    jokers cost $4, uncommon jokers cost $7, rare jokers cost $10, and legendary
    jokers cost $14.*)
let joker_cost (joker : Joker.t) =
  match Joker.rarity joker with
  | "Legendary" -> 14
  | "Rare" -> 10
  | "Uncommon" -> 7
  | _ -> 4

let planet_purchase () =
  let hand_list = load_file "data/hands.txt" in
  let rand_idx = Random.int (List.length hand_list - 3) in
  (* Note: 5OAK, flush house, flush five not supported *)
  let random_hand = List.nth hand_list rand_idx in
  PlanetCard.of_hand random_hand

let open_shop (money : int ref) (deck : Deck.t ref) (jokers : Joker.t array ref)
    : unit =
  let shopping = ref true in
  (* let purchases = ref [] in *)
  Unix.sleep 1;
  let joker_bought = ref false in
  let planet_card_bought = ref false in
  let random_joker = ref (generate_joker jokers (Random.int 100 + 1)) in
  let random_planet_card = ref (planet_purchase ()) in
  while !shopping do
    if !joker_bought = false && !planet_card_bought = false then
      random_joker := generate_joker jokers (Random.int 100 + 1);
    let joker_price = joker_cost !random_joker in
    if !joker_bought = false && !planet_card_bought = false then
      random_planet_card := planet_purchase ();
    print_endline
      "\n==================== WELCOME TO THE SHOP ====================";
    Printf.printf "💰 You have $%d\n" !money;
    print_endline "Choose an item to purchase:";
    print_endline "1. Card - $2";
    if !joker_bought then print_endline "Joker already purchased!"
    else
      print_endline
        ("2. "
        ^ Joker.to_string !random_joker
        ^ " - $" ^ string_of_int joker_price);
    if !planet_card_bought then print_endline "Planet card already purchased!"
    else
      print_endline
        ("3. "
        ^ PlanetCard.to_string !random_planet_card
        ^ " ("
        ^ PlanetCard.to_hand !random_planet_card
        ^ ") - $3");
    print_endline "4. Reroll";
    print_endline "5. Done shopping";
    print_string "> ";

    match read_line () with
    | "1" ->
        if !money < 2 then print_endline "Not enough money for a card"
        else money := !money - 2;
        let card = card_purchase () in
        (* purchases := DeckPurchase card :: !purchases; *)
        process_purchase (DeckPurchase card) deck jokers
    | "2" ->
        if !joker_bought then ()
        else (
          if !money < joker_price then
            print_endline "Not enough money for this joker"
          else money := !money - joker_price;
          (* purchases := JokerPurchase !random_joker :: !purchases; *)
          joker_bought := true;
          process_purchase (JokerPurchase !random_joker) deck jokers)
    | "3" ->
        if !planet_card_bought then ()
        else (
          if !money < 3 then print_endline "Not enough money for a planet card"
          else money := !money - 3;
          (* purchases := PlanetPurchase (PlanetCard.to_hand !random_planet_card
             |> Hand.create_hands) :: !purchases; *)
          planet_card_bought := true;
          process_purchase
            (PlanetPurchase
               (PlanetCard.to_hand !random_planet_card |> Hand.create_hands))
            deck jokers)
    | "4" ->
        if !money < 5 then print_endline "Not enough money to reroll"
        else money := !money - 5;
        joker_bought := false;
        planet_card_bought := false
    | "5" ->
        print_endline "Processing your purchases...";
        shopping := false
    | _ -> print_endline "Invalid option. Try again."
  done

(* Unix.sleep 2; List.iter (fun x -> process_purchases x deck jokers)
   !purchases; print_endline "All purchases processed. Thanks for shopping!" *)
