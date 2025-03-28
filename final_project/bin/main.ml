module Card = Final_project.Card
module Hand = Final_project.Hand
module Score = Final_project.Scoring

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

let generate_rand_hand () =
  Random.self_init ();
  let hand = ref [] in
  for _ = 0 to 7 do
    let rank = Random.int 13 + 1 in
    let suit =
      match Random.int 4 with
      | 0 -> "Spades"
      | 1 -> "Hearts"
      | 2 -> "Diamonds"
      | _ -> "Clubs"
    in
    let card = Card.of_pair (suit, rank) in
    hand := card :: !hand
  done;
  !hand

let rec get_user_selection cards =
  print_endline "Here are the available cards:";
  print_endline (card_list_printer cards);
  print_endline
    "Enter the indicies of up to 5 cards to score (whitespace separated, ex. \
     '1 2 3')\n";
  let input = read_line () in
  try
    let indices = input |> String.split_on_char ' ' |> List.map int_of_string in
    if List.length indices > 5 then (
      print_endline "You can only select up to 5 cards. Try again.\n";
      get_user_selection cards)
    else
      let selected_cards = List.map (List.nth cards) indices in
      selected_cards
  with
  | Failure _ ->
      print_endline "Invalid input. Please enter valid indicies.\n";
      get_user_selection cards
  | Invalid_argument _ ->
      print_endline "One or more indicies are out of range. Try again.\n";
      get_user_selection cards

let test_hand = [
  Card.of_pair ("Spades", 10);
  Card.of_pair ("Spades", 10);
  Card.of_pair ("Spades", 10);
  Card.of_pair ("Spades", 10);
  Card.of_pair ("Spades", 10);
  Card.of_pair ("Spades", 10);
  Card.of_pair ("Spades", 10);
]

let () =
  let cards = generate_rand_hand () in
  let selected_hand = get_user_selection cards in
  print_endline "You selected the following card:\n";
  print_endline (card_list_printer selected_hand);

  if selected_hand = [] then print_endline "No cards selected."
  else
    try
      let score = Score.score_played_cards selected_hand in
      let hand_type =
        Hand.highest_hand selected_hand |> fst |> Hand.played_hand_type
      in
      Printf.printf "The score for your selected hand is: %d\n" score;
      Printf.printf "The type of hand is: %s\n" hand_type
    with
    | Failure msg -> Printf.printf "Scoring failed: %s\n" msg
    | _ -> print_endline "Unknown error has occured."
