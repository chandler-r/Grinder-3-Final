module Card = Final_project.Card
module Hand = Final_project.Hand
module Score = Final_project.Scoring

let card_list_printer cards =
  if List.length cards = 0 then "None"
  else
    let center contents width = 
      let padding = width - String.length contents in
      let left = padding/2 in
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
  for _ = 0 to 8 do
    let rank = Random.int 13 + 1 in
    let suit = match Random.int 4 with
      | 0 -> "Spades"
      | 1 -> "Hearts"
      | 2 -> "Diamonds"
      | _ -> "Clubs"
    in
    let card = Card.of_pair (suit, rank) in
    hand := card :: !hand
  done;
  !hand
  


let () =
  match Sys.argv with
  | _ -> print_endline (card_list_printer (generate_rand_hand ()))
