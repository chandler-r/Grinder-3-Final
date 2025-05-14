open Final_project
(* module Card = Final_project.Card module Hand = Final_project.Hand module
   Score = Final_project.Scoring module Deck = Final_project.Deck module Joker =
   Final_project.Joker module Shop = Final_project.Shop *)

open ANSITerminal

let curr_level = Level.start_level ()
let money = Money.money ()
let deck = ref (Deck.init ())
let deck_length = ref 52
(* let small_blind = ref 300 let big_blind = ref 600 let boss_blind = ref 900 *)

(* let jokers = ref [| Joker.of_string "Misprint"; Joker.of_string "OddTodd";
   Joker.of_string "EvenSteven"; Joker.of_string "Bloodstone"; Joker.of_string
   "TheDuo"; |] *)

let jokers = ref [||]

let card_list_printer cards =
  if List.length cards = 0 then "None"
  else
    let center contents width =
      let padding = width - String.length contents in
      let left = padding / 2 in
      let right = padding - left in
      String.make left ' ' ^ contents ^ String.make right ' '
    in
    let card_to_print card =
      let rank, suit =
        match String.split_on_char ' ' (Card.to_string card) with
        | [ r; _; s ] -> (r, s)
        | _ -> failwith "Invalid Card Format"
      in
      [
        "┌──────────┐";
        Printf.sprintf "|%-10s     |" suit;
        "|          |";
        Printf.sprintf "|%-10s|" (center rank 10);
        "|          |";
        Printf.sprintf "|    %10s |" suit;
        "└──────────┘";
      ]
    in
    let buffer = Buffer.create 256 in
    let card_rows = List.map card_to_print cards in
    if card_rows = [] then failwith "No cards to display.";
    let num_rows = List.length (List.hd card_rows) in
    for i = 0 to num_rows - 1 do
      List.iter
        (fun card ->
          Buffer.add_string buffer (List.nth card i);
          Buffer.add_string buffer "  ")
        card_rows;
      Buffer.add_char buffer '\n'
    done;
    Buffer.contents buffer

(* Depreciated

   let generate_rand_hand () = Random.self_init (); let hand = ref [] in for _ =
   0 to 7 do let rank = Random.int 13 + 1 in let suit = match Random.int 4 with
   | 0 -> "Spades" | 1 -> "Hearts" | 2 -> "Diamonds" | _ -> "Clubs" in let card
   = Card.of_pair (suit, rank) in hand := card :: !hand done; !hand *)

let rec get_user_selection cards =
  print_endline "Here are the available cards:";
  print_endline (card_list_printer cards);
  print_endline
    "Enter the indices of up to 5 cards (whitespace separated, ex. '1 2 3')\n";
  let input = read_line () in
  try
    let indices =
      input |> String.split_on_char ' ' |> List.map int_of_string
      |> List.map (fun x -> x - 1)
    in
    if List.length indices > 5 then (
      print_endline "\nYou can only select up to 5 cards. Try again.\n";
      get_user_selection cards)
    else if List.length indices < 1 then (
      print_endline "\nYou must choose at least 1 card.";
      get_user_selection cards)
    else if List.length indices <> List.length (List.sort_uniq compare indices)
    then (
      print_endline "\nDuplicate indices detected. Please try again.\n";
      get_user_selection cards)
    else
      let selected_cards = List.map (List.nth cards) indices in
      selected_cards
  with
  | Failure _ ->
      print_endline "\nInvalid input. Please enter valid indices.\n";
      get_user_selection cards
  | Invalid_argument _ ->
      print_endline "\nOne or more indices are out of range. Try again.\n";
      get_user_selection cards

let test_hand =
  [
    Card.of_pair ("Spades", 10);
    Card.of_pair ("Spades", 10);
    Card.of_pair ("Spades", 10);
    Card.of_pair ("Spades", 10);
    Card.of_pair ("Spades", 10);
    Card.of_pair ("Spades", 10);
    Card.of_pair ("Spades", 10);
  ]

let play_blind level hands discards =
  let hands_left = ref hands in
  let discards_left = ref discards in
  let cumulative_score = ref 0 in
  let curr_cards = ref [] in

  while !hands_left > 0 do
    Printf.printf "Your current blind: %S" (Level.to_string curr_level);

    Printf.printf
      "\n\
       You must meet or exceed the following score to beat this blind. \n\n\
       Blind Threshold: %d\n\n"
      (Level.target_score level);
    Printf.printf "Current Score: %d\n\n" !cumulative_score;
    Printf.printf "Remaining Hands: %d, Remaining Discards: %d\n\n" !hands_left
      !discards_left;
    let deck_copy = Deck.copy_deck !deck in
    if !curr_cards = [] then curr_cards := Deck.draw_cards deck_copy 8;

    let selected_hand = get_user_selection !curr_cards in
    print_endline "You selected the following card(s):\n";
    print_endline (card_list_printer selected_hand);

    print_endline "\n1. Play Hand or 2. Discard Cards?";
    print_endline "\nEnter your choice (1 or 2)\n";
    match read_line () with
    | "1" -> (
        try
          let score = Scoring.score_played_cards selected_hand !jokers in
          cumulative_score := !cumulative_score + score;

          if !cumulative_score >= Level.target_score level then (
            print_endline
              "\n\
               You beat the blind! Enjoy your spoils and continue your journey.\n";
            Money.end_of_round level (!hands_left - 1) money;
            Printf.printf "You current money: %d\n" !money;
            (* Opens the shop allowing you to buy stuff. *)
            Shop.open_shop money deck jokers;
            hands_left := 0);
          curr_cards :=
            List.filter
              (fun card -> not (List.mem card selected_hand))
              !curr_cards;
          let cards_to_draw = 8 - List.length !curr_cards in
          let new_cards = Deck.draw_cards deck_copy cards_to_draw in
          curr_cards := !curr_cards @ new_cards;
          hands_left := !hands_left - 1
        with
        | Failure msg ->
            Printf.printf "Internal Failure: %s\n" msg;
            Shop.open_shop money deck jokers
        | _ ->
            print_endline "Unknown error has occured.";
            Shop.open_shop money deck jokers)
    | "2" when !discards_left > 0 ->
        ANSITerminal.erase ANSITerminal.Screen;
        let num_cards = List.length selected_hand in
        if num_cards > 0 && num_cards <= 5 then (
          discards_left := !discards_left - 1;
          let new_cards = Deck.draw_cards deck_copy num_cards in
          let new_card_index = ref 0 in
          curr_cards :=
            List.mapi
              (fun i card ->
                if List.mem card selected_hand then (
                  let new_card = List.nth new_cards !new_card_index in
                  incr new_card_index;
                  new_card)
                else card)
              !curr_cards)
        else print_endline "Invalid number of cards"
    | "2" -> print_endline "No discards left."
    | _ -> print_endline "Invalid choice. Please try again."
  done;
  if !cumulative_score < Level.target_score level then (
    print_endline "You failed to meet the blind. YOU LOSE!";
    exit 0)

let rec game_loop () =
  ANSITerminal.erase ANSITerminal.Screen;
  (* let blind_threshold = Blind.choose_blind () in *)
  let hands = 4 in
  let discards = 3 in
  (* ANSITerminal.erase ANSITerminal.Screen; *)
  play_blind curr_level hands discards;
  try
    Level.incr_level curr_level;
    game_loop ()
  with Level.GameOver -> Printf.printf "Great Job! You won!"

let () = game_loop ()
