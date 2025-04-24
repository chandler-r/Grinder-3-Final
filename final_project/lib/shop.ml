type t =
  | Deck_purchase of Card.t
  | Joker_purchase of Joker.t
  | Planet_card_purchase of Hand.hands

(** Write doc. AF and RI Baloney. *)

(** [process_deck_purchase deck purchase] adds the card [purchase] into the deck
    [deck]. Modifies the global reference. *)
let process_deck_purchase (deck : Deck.t ref) (purchase : Card.t) =
  deck := Deck.add_card !deck purchase

(** Write doc when done*)
let process_joker_purchase (jokers : Joker.t array ref) (purchase : Joker.t) =
  failwith "Not Implemented."

(** [process_planet_card purchase] levels up the hand associated with
    [purchase].*)
let process_planet_card (purchase : Hand.hands) = Scoring.level_up_hand purchase

let process_purchases (purchase : t) (deck : Deck.t ref)
    (jokers : Joker.t array ref) =
  match purchase with
  | Deck_purchase purchase -> process_deck_purchase deck purchase
  | Joker_purchase purchase -> process_joker_purchase jokers purchase
  | Planet_card_purchase purchase -> process_planet_card purchase

let dummy_card_purchase () = Card.of_pair ("spades", 4)
let dummy_joker_purchase () = Joker.of_string "Scholar"
let dummy_planet_purchase () = Hand.create_hands "high card"

let open_shop (money : int ref) (deck : Deck.t ref) (jokers : Joker.t array ref)
    : unit =
  let shopping = ref true in
  let purchases = ref [] in
  Unix.sleep 1;
  while !shopping do
    print_endline
      "\n==================== WELCOME TO THE SHOP ====================";
    Printf.printf "💰 You have $%d\n" !money;
    print_endline "Choose an item to purchase:";
    print_endline "1. Card - $2";
    print_endline "2. Joker - $5";
    print_endline "3. Planet card (high card) - $1";
    print_endline "4. Done shopping";
    print_string "> ";

    match read_line () with
    | "1" ->
        if !money < 2 then print_endline "Not enough money for a card"
        else money := !money - 2;
        let card = dummy_card_purchase () in
        purchases := Deck_purchase card :: !purchases;
        print_endline "Card added to cart!"
    | "2" ->
        if !money < 5 then print_endline "Not enough money for a joker"
        else money := !money - 5;
        let joker = dummy_joker_purchase () in
        purchases := Joker_purchase joker :: !purchases;
        print_endline "Joker added to cart!"
    | "3" ->
        if !money < 1 then print_endline "Not enough money for a planet card"
        else money := !money - 1;
        let planet_card = dummy_planet_purchase () in
        purchases := Planet_card_purchase planet_card :: !purchases;
        print_endline "Card added to cart!"
    | "4" ->
        print_endline "Processing your purchases...";
        shopping := false
    | _ -> print_endline "Invalid option. Try again."
  done;

  Unix.sleep 2;
  List.iter (fun x -> process_purchases x deck jokers) !purchases;
  print_endline "All purchases processed. Thanks for shopping!"
