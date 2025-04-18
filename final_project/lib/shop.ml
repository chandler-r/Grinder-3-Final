type t =
  | Deck_purchase of Card.t
  | Joker_purchase of Joker.t

(** Write doc. AF and RI Baloney. *)

(** Write doc when done. *)
let process_deck_purchase (deck : Deck.t ref) (purchase : Card.t) =
  failwith "Not Implemented."

(** Write doc when done*)
let process_joker_purchase (jokers : Joker.t array ref) (purchase : Joker.t) =
  failwith "Not Implemented."

let process_purchases (purchase : t) (deck : Deck.t ref)
    (jokers : Joker.t array ref) =
  match purchase with
  | Deck_purchase purchase -> process_deck_purchase deck purchase
  | Joker_purchase purchase -> process_joker_purchase jokers purchase

let dummy_card () = Card.of_pair ("spades", 4)
let dummy_joker () = Joker.make_joker ()

let open_shop (money : int ref) (deck : Deck.t ref) (jokers : Joker.t array ref)
    : unit =
  let shopping = ref true in
  let purchases = ref [] in

  while !shopping do
    Unix.sleep 3;
    print_endline
      "\n==================== WELCOME TO THE SHOP ====================";
    Printf.printf "💰 You have $%d\n" !money;
    print_endline "Choose an item to purchase:";
    print_endline "1. Card - $2";
    print_endline "2. Joker - $5";
    print_endline "3. Done shopping";
    print_string "> ";

    match read_line () with
    | "1" ->
        if !money < 2 then print_endline "Not enough money for a card!"
        else begin
          money := !money - 2;
          let card = dummy_card () in
          purchases := Deck_purchase card :: !purchases;
          print_endline "Card added to cart!"
        end
    | "2" ->
        if !money < 5 then print_endline "Not enough money for a joker!"
        else begin
          money := !money - 5;
          let joker = dummy_joker () in
          purchases := Joker_purchase joker :: !purchases;
          print_endline "Joker added to cart!"
        end
    | "3" ->
        print_endline "Processing your purchases...";
        shopping := false
    | _ -> print_endline "Invalid option. Try again."
  done;

  List.iter (fun p -> process_purchases p deck jokers) (List.rev !purchases);
  print_endline "All purchases processed. Thanks for shopping!"
