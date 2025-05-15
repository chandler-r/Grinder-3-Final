[@@@coverage exclude_file]

open OUnit2
open Final_project

let card_list_printer cards =
  if List.length cards = 0 then "None"
  else
    List.fold_left
      (fun str card -> str ^ " " ^ Card.to_string card ^ ";")
      "" cards
    |> fun x -> "[" ^ String.sub x 0 (String.length x - 1) ^ " ]"

let joker_list_printer jokers =
  if Array.length jokers = 0 then "None"
  else
    Array.fold_left
      (fun str joker -> str ^ " " ^ Joker.to_string joker ^ ";")
      "" jokers
    |> fun x -> "[" ^ String.sub x 0 (String.length x - 1) ^ " ]"

let create_hand_type_test expected cards =
  "When playing the cards " ^ card_list_printer cards ^ ", the hand type "
  ^ expected ^ " is expected."
  >:: fun _ ->
  assert_equal expected
    (Hand.highest_hand cards |> fst |> Hand.played_hand_type)
    ~printer:Fun.id

let create_basic_scoring_test expected cards =
  "Playing the cards " ^ card_list_printer cards ^ " gives the score "
  ^ string_of_int expected
  >:: fun _ ->
  assert_equal expected
    (fst (Scoring.score_played_cards cards [||]))
    ~printer:string_of_int

let create_joker_scoring_test expected cards jokers =
  "Playing the cards " ^ card_list_printer cards ^ " with jokers "
  ^ joker_list_printer jokers ^ " gives the score " ^ string_of_int expected
  >:: fun _ ->
  assert_equal expected
    (fst (Scoring.score_played_cards cards jokers))
    ~printer:string_of_int

let card_tests =
  [
    ( "simple card test number" >:: fun _ ->
      assert_equal
        (Card.number (Card.of_pair ("Spades", 5)))
        5 ~printer:string_of_int );
    ( "simple card test suit" >:: fun _ ->
      assert_equal (Card.suit (Card.of_pair ("Spades", 5))) "♠️" ~printer:Fun.id
    );
    ( "simple card test raise bad suit" >:: fun _ ->
      assert_raises
        (Invalid_argument
           "Suit must be one of \"Spades\", \"Hearts\", \"Diamonds\", or \
            \"Clubs\".") (fun _ -> Card.of_pair ("Spade", 5)) );
    ( "simple card test raise bad number (low)" >:: fun _ ->
      assert_raises
        (Invalid_argument "Rank must be between 1 and 14 (inclusive).")
        (fun _ -> Card.of_pair ("Spades", 0)) );
    ( "simple card test raise bad number (high)" >:: fun _ ->
      assert_raises
        (Invalid_argument "Rank must be between 1 and 14 (inclusive).")
        (fun _ -> Card.of_pair ("Spades", 15)) );
    ( "simple card test raise bad suit and bad number" >:: fun _ ->
      assert_raises
        (Invalid_argument "Rank must be between 1 and 14 (inclusive).")
        (fun _ -> Card.of_pair ("Spade", 0)) );
    ( "simple card to_string and of_pair test" >:: fun _ ->
      assert_equal
        (Card.to_string (Card.of_pair ("Spades", 5)))
        "Five of ♠️" ~printer:Fun.id );
  ]

let planet_cards =
  [
    "Mercury";
    "Venus";
    "Earth";
    "Mars";
    "Jupiter";
    "Saturn";
    "Uranus";
    "Neptune";
    "Pluto";
  ]

let hands =
  [
    "pair";
    "three of a kind";
    "full house";
    "four of a kind";
    "flush";
    "straight";
    "two pair";
    "straight flush";
    "high card";
    "five of a kind";
    "flush house";
    "flush five";
  ]

let planet_str_helper card =
  card ^ " should have a planet card named after itself." >:: fun _ ->
  assert_equal card
    (PlanetCard.to_string (PlanetCard.of_string card))
    ~printer:Fun.id

let planet_card_str_tests = List.map planet_str_helper planet_cards
let print_mc (m, c) = string_of_float m ^ ", " ^ string_of_int c

let planet_effect_helper success card hand =
  let mult, chips = (ref 2.5, ref 100) in
  let () =
    PlanetCard.use_planet_card
      (PlanetCard.of_string card)
      (Hand.create_hands hand) mult chips
  in
  let addM, addC =
    if not success then (0., 0)
    else
      match card with
      | "Mercury" -> (1., 15)
      | "Venus" -> (2., 20)
      | "Earth" -> (2., 25)
      | "Mars" -> (3., 30)
      | "Jupiter" -> (2., 15)
      | "Saturn" -> (3., 30)
      | "Uranus" -> (1., 20)
      | "Neptune" -> (4., 40)
      | "Pluto" -> (1., 10)
      | _ ->
          failwith
            "Only Mercury through Pluto planet cards are supported currently."
  in

  "Planet card " ^ card ^ " with hand " ^ hand
  ^ " will have correct effect on initial 2.5 mult and 100 chips."
  >:: fun _ ->
  assert_equal (2.5 +. addM, 100 + addC) (!mult, !chips) ~printer:print_mc

let planet_card_effect_tests =
  List.flatten
    (List.mapi
       (fun i c ->
         List.mapi
           (fun j h ->
             if i = j then planet_effect_helper true c h
             else planet_effect_helper false c h)
           hands)
       planet_cards)

let planet_of_hand_helper hand =
  "to_hand of of_hand should not modify the hand, with the exception of \
   possible capitalization (" ^ hand ^ ")"
  >:: fun _ ->
  assert_equal hand
    (hand |> PlanetCard.of_hand |> PlanetCard.to_hand |> String.lowercase_ascii)
    ~printer:Fun.id

let planet_of_hand_tests =
  List.map planet_of_hand_helper (List.filteri (fun i _ -> i < 9) hands)

let deck_draw_helper n =
  let deck = Deck.init () in
  try
    let lst = Deck.draw_cards deck n in
    52 = List.length (Deck.to_list deck) && n = List.length lst
  with
  | Failure s ->
      52 = List.length (Deck.to_list deck) && s = "Not enough cards in the deck"
  | _ -> false

let deck_tests =
  [
    ( "The size of an initialized deck should be 52" >:: fun _ ->
      assert_equal 52 (List.length (Deck.to_list (Deck.init ()))) );
    (let t =
       QCheck2.Test.make ~count:100
         (QCheck2.Gen.int_range ~origin:50 0 60)
         deck_draw_helper
     in
     QCheck_runner.to_ounit2_test t);
  ]

let hand_tests =
  (* Note: ocamlformat separates each list element by line *)
  [
    create_hand_type_test "high card"
      [
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Hearts", 7);
        Card.of_pair ("Clubs", 6);
        Card.of_pair ("Hearts", 4);
        Card.of_pair ("Diamonds", 2);
      ];
    create_hand_type_test "pair"
      [
        Card.of_pair ("Diamonds", 7);
        Card.of_pair ("Hearts", 7);
        Card.of_pair ("Clubs", 6);
        Card.of_pair ("Hearts", 4);
        Card.of_pair ("Diamonds", 2);
      ];
    create_hand_type_test "two pair"
      [
        Card.of_pair ("Diamonds", 7);
        Card.of_pair ("Hearts", 7);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Hearts", 4);
        Card.of_pair ("Diamonds", 2);
      ];
    create_hand_type_test "three of a kind"
      [
        Card.of_pair ("Diamonds", 7);
        Card.of_pair ("Hearts", 7);
        Card.of_pair ("Clubs", 6);
        Card.of_pair ("Hearts", 7);
        Card.of_pair ("Diamonds", 2);
      ];
    create_hand_type_test "straight"
      [
        Card.of_pair ("Diamonds", 2);
        Card.of_pair ("Hearts", 3);
        Card.of_pair ("Clubs", 6);
        Card.of_pair ("Hearts", 5);
        Card.of_pair ("Diamonds", 4);
      ];
    create_hand_type_test "straight"
      [
        Card.of_pair ("Diamonds", 2);
        Card.of_pair ("Hearts", 3);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Hearts", 5);
        Card.of_pair ("Diamonds", 4);
      ];
    create_hand_type_test "high card"
      [
        Card.of_pair ("Diamonds", 2);
        Card.of_pair ("Hearts", 3);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Hearts", 6);
        Card.of_pair ("Diamonds", 4);
      ];
    create_hand_type_test "flush"
      [
        Card.of_pair ("Hearts", 2);
        Card.of_pair ("Hearts", 3);
        Card.of_pair ("Hearts", 14);
        Card.of_pair ("Hearts", 6);
        Card.of_pair ("Hearts", 4);
      ];
    create_hand_type_test "high card"
      [
        Card.of_pair ("Diamonds", 2);
        Card.of_pair ("Hearts", 3);
        Card.of_pair ("Hearts", 14);
        Card.of_pair ("Hearts", 6);
        Card.of_pair ("Hearts", 4);
      ];
    create_hand_type_test "flush"
      [
        Card.of_pair ("Diamonds", 2);
        Card.of_pair ("Diamonds", 3);
        Card.of_pair ("Diamonds", 14);
        Card.of_pair ("Diamonds", 14);
        Card.of_pair ("Diamonds", 4);
      ];
    create_hand_type_test "flush"
      [
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 3);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Clubs", 4);
      ];
    create_hand_type_test "flush"
      [
        Card.of_pair ("Spades", 14);
        Card.of_pair ("Spades", 3);
        Card.of_pair ("Spades", 14);
        Card.of_pair ("Spades", 14);
        Card.of_pair ("Spades", 4);
      ];
    create_hand_type_test "full house"
      [
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Diamonds", 14);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Clubs", 4);
      ];
    create_hand_type_test "four of a kind"
      [
        Card.of_pair ("Diamonds", 14);
        Card.of_pair ("Spades", 14);
        Card.of_pair ("Spades", 14);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Hearts", 4);
      ];
    create_hand_type_test "straight flush"
      [
        Card.of_pair ("Spades", 14);
        Card.of_pair ("Spades", 2);
        Card.of_pair ("Spades", 5);
        Card.of_pair ("Spades", 3);
        Card.of_pair ("Spades", 4);
      ];
    create_hand_type_test "five of a kind"
      [
        Card.of_pair ("Diamonds", 4);
        Card.of_pair ("Spades", 4);
        Card.of_pair ("Spades", 4);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Hearts", 4);
      ];
    create_hand_type_test "flush house"
      [
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Clubs", 4);
      ];
    create_hand_type_test "flush five"
      [
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 4);
      ];
    create_hand_type_test "two pair"
      [
        Card.of_pair ("Diamonds", 13);
        Card.of_pair ("Clubs", 13);
        Card.of_pair ("Hearts", 11);
        Card.of_pair ("Hearts", 11);
        Card.of_pair ("Clubs", 9);
      ];
    create_hand_type_test "straight"
      [
        Card.of_pair ("Diamonds", 10);
        Card.of_pair ("Clubs", 13);
        Card.of_pair ("Hearts", 11);
        Card.of_pair ("Hearts", 12);
        Card.of_pair ("Clubs", 1);
      ];
  ]

let basic_scoring_tests =
  [
    (* Used this website to calculate played hand scores:
       https://efhiii.github.io/balatro-calculator/?h=AACgAJGAoEQIACA *)
    (* high card - ace (5 + 11) *)
    create_basic_scoring_test 16
      [
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Hearts", 7);
        Card.of_pair ("Clubs", 6);
        Card.of_pair ("Hearts", 4);
        Card.of_pair ("Diamonds", 2);
      ];
    (* pair - 7 ((10 + 7 * 2) * 2) *)
    create_basic_scoring_test 48
      [
        Card.of_pair ("Diamonds", 7);
        Card.of_pair ("Hearts", 7);
        Card.of_pair ("Clubs", 6);
        Card.of_pair ("Hearts", 4);
        Card.of_pair ("Diamonds", 2);
      ];
    (* two pair - 7, 4 ((20 + 7 * 2 + 4 * 2) * 2) *)
    create_basic_scoring_test 84
      [
        Card.of_pair ("Diamonds", 7);
        Card.of_pair ("Hearts", 7);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Hearts", 4);
        Card.of_pair ("Diamonds", 2);
      ];
    (* three of a kind - 7 ((30 + 7 * 3) * 3) *)
    create_basic_scoring_test 153
      [
        Card.of_pair ("Diamonds", 7);
        Card.of_pair ("Hearts", 7);
        Card.of_pair ("Clubs", 6);
        Card.of_pair ("Hearts", 7);
        Card.of_pair ("Diamonds", 2);
      ];
    (* three of a kind (same calculation as above) *)
    create_basic_scoring_test 153
      [
        Card.of_pair ("Diamonds", 7);
        Card.of_pair ("Hearts", 7);
        Card.of_pair ("Clubs", 6);
        Card.of_pair ("Spades", 7);
        Card.of_pair ("Diamonds", 2);
      ];
    (* straight - 2-6 ((30 + 2 + 3 + 4 + 5 + 6) * 4) *)
    create_basic_scoring_test 200
      [
        Card.of_pair ("Diamonds", 2);
        Card.of_pair ("Hearts", 3);
        Card.of_pair ("Clubs", 6);
        Card.of_pair ("Hearts", 5);
        Card.of_pair ("Diamonds", 4);
      ];
    (* straight - A-5 ((30 + 11 + 2 + 3 + 4 + 5) * 4) *)
    create_basic_scoring_test 220
      [
        Card.of_pair ("Diamonds", 2);
        Card.of_pair ("Hearts", 3);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Hearts", 5);
        Card.of_pair ("Diamonds", 4);
      ];
    (* high card (calculation above) *)
    create_basic_scoring_test 16
      [
        Card.of_pair ("Diamonds", 2);
        Card.of_pair ("Hearts", 3);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Hearts", 6);
        Card.of_pair ("Diamonds", 4);
      ];
    (* flush - hearts ((35 + 2 + 3 + 11 + 4 + 6) * 4) *)
    create_basic_scoring_test 244
      [
        Card.of_pair ("Hearts", 2);
        Card.of_pair ("Hearts", 3);
        Card.of_pair ("Hearts", 14);
        Card.of_pair ("Hearts", 6);
        Card.of_pair ("Hearts", 4);
      ];
    (* high card (calculation above) *)
    create_basic_scoring_test 13
      [
        Card.of_pair ("Diamonds", 2);
        Card.of_pair ("Hearts", 3);
        Card.of_pair ("Hearts", 8);
        Card.of_pair ("Hearts", 6);
        Card.of_pair ("Hearts", 4);
      ];
    (* flush - diamonds ((35 + 2 + 3 + 11 + 11 + 4) * 4) *)
    create_basic_scoring_test 264
      [
        Card.of_pair ("Diamonds", 2);
        Card.of_pair ("Diamonds", 3);
        Card.of_pair ("Diamonds", 14);
        Card.of_pair ("Diamonds", 14);
        Card.of_pair ("Diamonds", 4);
      ];
    (* flush - clubs ((35 + 4 + 3 + 11 + 11 + 4) * 4) *)
    create_basic_scoring_test 272
      [
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 3);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Clubs", 4);
      ];
    (* flush - spades ((35 + 11 + 3 + 11 + 11 + 4) * 4) *)
    create_basic_scoring_test 300
      [
        Card.of_pair ("Spades", 14);
        Card.of_pair ("Spades", 3);
        Card.of_pair ("Spades", 14);
        Card.of_pair ("Spades", 14);
        Card.of_pair ("Spades", 4);
      ];
    (* full house - 4, 14 ((40 + 4 * 3 + 14 * 2) * 4) *)
    create_basic_scoring_test 296
      [
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Diamonds", 14);
        Card.of_pair ("Clubs", 4);
      ];
    (* four of a kind - 14 ((60 + 11 * 4) * 7) *)
    create_basic_scoring_test 728
      [
        Card.of_pair ("Diamonds", 14);
        Card.of_pair ("Spades", 14);
        Card.of_pair ("Spades", 14);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Hearts", 4);
      ];
    (* straight flush - A-5 ((100 + 11 + 2 + 3 + 4 + 5) * 8) *)
    create_basic_scoring_test 1000
      [
        Card.of_pair ("Spades", 14);
        Card.of_pair ("Spades", 2);
        Card.of_pair ("Spades", 5);
        Card.of_pair ("Spades", 3);
        Card.of_pair ("Spades", 4);
      ];
    (* five of a kind - 4 ((120 + 4 * 5) * 12) *)
    create_basic_scoring_test 1680
      [
        Card.of_pair ("Diamonds", 4);
        Card.of_pair ("Spades", 4);
        Card.of_pair ("Spades", 4);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Hearts", 4);
      ];
    (* flush house - 4,14 ((140 + 11 * 2 + 4 * 3) * 14) *)
    create_basic_scoring_test 2436
      [
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Clubs", 4);
      ];
    (* flush five - 4 ((160 + 4 * 5) * 16) *)
    create_basic_scoring_test 2880
      [
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 4);
      ];
  ]

let joker_scoring_tests =
  [
    (* Used this website to calculate played hand scores:
       https://efhiii.github.io/balatro-calculator/?h=AACgAJGAoEQIACA *)
    create_joker_scoring_test 1265
      [
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Hearts", 2);
        Card.of_pair ("Spades", 3);
        Card.of_pair ("Hearts", 4);
        Card.of_pair ("Diamonds", 5);
      ]
      [|
        Joker.of_string "Joker";
        Joker.of_string "GreedyJoker";
        Joker.of_string "LustyJoker";
        Joker.of_string "WrathfulJoker";
        Joker.of_string "GluttonousJoker";
      |];
    create_joker_scoring_test 9159
      [
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Hearts", 14);
        Card.of_pair ("Spades", 14);
      ]
      [|
        Joker.of_string "JollyJoker";
        Joker.of_string "ZanyJoker";
        Joker.of_string "WilyJoker";
        Joker.of_string "SlyJoker";
        Joker.of_string "HalfJoker";
      |];
    create_joker_scoring_test 18910
      [
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Clubs", 2);
        Card.of_pair ("Clubs", 3);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 5);
      ]
      [|
        Joker.of_string "CrazyJoker";
        Joker.of_string "DrollJoker";
        Joker.of_string "DeviousJoker";
        Joker.of_string "CraftyJoker";
        Joker.of_string "Fibonacci";
      |];
    create_joker_scoring_test 4680
      [
        Card.of_pair ("Spades", 3);
        Card.of_pair ("Hearts", 3);
        Card.of_pair ("Clubs", 8);
        Card.of_pair ("Diamonds", 8);
        Card.of_pair ("Clubs", 5);
      ]
      [|
        Joker.of_string "MadJoker";
        Joker.of_string "CleverJoker";
        Joker.of_string "EvenSteven";
        Joker.of_string "OddTodd";
        Joker.of_string "Arrowhead";
      |];
    create_joker_scoring_test 178524
      [
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Clubs", 10);
        Card.of_pair ("Clubs", 11);
        Card.of_pair ("Clubs", 12);
        Card.of_pair ("Clubs", 13);
      ]
      [|
        Joker.of_string "Scholar";
        Joker.of_string "OnyxAgate";
        Joker.of_string "TheOrder";
        Joker.of_string "TheTribe";
        Joker.of_string "Triboulet";
      |];
    create_joker_scoring_test 80640
      [
        Card.of_pair ("Hearts", 10);
        Card.of_pair ("Hearts", 10);
        Card.of_pair ("Hearts", 10);
        Card.of_pair ("Hearts", 10);
        Card.of_pair ("Hearts", 10);
      ]
      [|
        Joker.of_string "TheDuo";
        Joker.of_string "TheTrio";
        Joker.of_string "TheFamily";
      |];
  ]

(* Deck tests *)
let dummy_deck = Deck.init ()

let add_card_check n =
  let len = List.length (Deck.to_list (Deck.copy_deck dummy_deck)) in
  if n > 0 && n <= 14 then
    let deck = Deck.add_card dummy_deck (Card.of_pair ("Hearts", n)) in
    List.length (Deck.to_list deck) = len + 1
  else true

let add_card_test =
  QCheck.(Test.make ~count:10 ~name:"add card to deck" small_nat add_card_check)

let remove_card_check n =
  let len = List.length (Deck.to_list (Deck.copy_deck dummy_deck)) in
  if n > 0 && n <= 14 then
    let deck = Deck.remove_card dummy_deck (Card.of_pair ("Hearts", n)) in
    List.length (Deck.to_list deck) = len - 1
  else true

let remove_card_test =
  QCheck.(
    Test.make ~count:10 ~name:"remove card from deck" small_nat
      remove_card_check)

let dummy_hand = Hand.to_hand (Deck.draw_cards dummy_deck 5)

let rec pick_n n hand =
  let card_list = Hand.to_list hand in
  match n with
  | 0 -> []
  | _ -> List.nth card_list (n - 1) :: pick_n (n - 1) hand

(* Hand (play/discard) tests *)
let play_check num_cards =
  if num_cards > 5 || num_cards = 0 then true
  else
    let play, (hand, sth) =
      Hand.play
        (Hand.to_hand (pick_n num_cards dummy_hand))
        dummy_hand dummy_deck
    in
    List.length (Hand.to_list play) = 5

let play_test = QCheck.(Test.make ~count:10 ~name:"play" small_nat play_check)

let discard_check num_cards =
  if num_cards > 5 || num_cards = 0 then true
  else
    let disc =
      Hand.discard
        (Hand.to_hand (pick_n num_cards dummy_hand))
        dummy_hand dummy_deck
    in
    List.length (Hand.to_list disc) = 5

let discard_test =
  QCheck.(Test.make ~count:10 ~name:"discard" small_nat discard_check)

let play_disc_add_tests =
  List.map QCheck_runner.to_ounit2_test
    [ play_test; discard_test; add_card_test; remove_card_test ]

(* Money tests *)
let pay_check amount =
  let curr_money = ref 4 in
  if amount <= 4 then (
    Money.pay amount curr_money;
    !curr_money = 4 - amount)
  else true

let pay_test = QCheck.(Test.make ~count:10 ~name:"pay" small_nat pay_check)

let overdraw_check amount =
  let curr_money = ref 4 in
  if amount > 4 then
    try
      Money.pay amount curr_money;
      false
    with Money.InsufficientFunds -> true
  else true

let overdraw_test =
  QCheck.(Test.make ~count:10 ~name:"overdraw" small_nat overdraw_check)

let pay_tests =
  List.map QCheck_runner.to_ounit2_test [ pay_test; overdraw_test ]

let end_of_round_test (blind, starting_cash, expected_output) =
  "Expected "
  ^ string_of_int expected_output
  ^ " for level " ^ string_of_int blind ^ " and starting cash "
  ^ string_of_int starting_cash
  >:: fun _ ->
  let curr_money = ref starting_cash in
  let level = Level.start_level () in
  if blind = 1 then Level.incr_level level
  else if blind = 2 then (
    Level.incr_level level;
    Level.incr_level level)
  else if blind = 3 then (
    Level.incr_level level;
    Level.incr_level level;
    Level.incr_level level)
  else ();
  Money.end_of_round level 0 curr_money;
  assert_equal expected_output !curr_money ~printer:string_of_int

let end_of_round_tests =
  [
    end_of_round_test (0, 5, 9);
    end_of_round_test (0, 4, 7);
    end_of_round_test (3, 4, 7);
    end_of_round_test (1, 4, 8);
    end_of_round_test (2, 4, 9);
    end_of_round_test (0, 30, 38);
  ]

let level_tests =
  let incr_blind curr =
    match !curr with
    | "Small" -> curr := "Big"
    | "Big" -> curr := "Boss"
    | "Boss" -> curr := "Small"
    | _ -> failwith "Blind can only be one of 'Small'. 'Big', or 'Boss'."
  in
  let incr_target target =
    match !target with
    (* ante 1 *)
    | 300 -> target := 450
    | 450 -> target := 600
    | 600 -> target := 800
    (* ante 2 *)
    | 800 -> target := 1200
    | 1200 -> target := 1600
    | 1600 -> target := 2000
    (* ante 3 *)
    | 2000 -> target := 3000
    | 3000 -> target := 4000
    | 4000 -> target := 5000
    (* ante 4 *)
    | 5000 -> target := 7500
    | 7500 -> target := 10_000
    | 10_000 -> target := 11_000
    (* ante 5 *)
    | 11_000 -> target := 16_500
    | 16_500 -> target := 22_000
    | 22_000 -> target := 20_000
    (* ante 6 *)
    | 20_000 -> target := 30_000
    | 30_000 -> target := 40_000
    | 40_000 -> target := 35_000
    (* ante 7 *)
    | 35_000 -> target := 52_500
    | 52_500 -> target := 70_000
    | 70_000 -> target := 50_000
    (* ante 8 *)
    | 50_000 -> target := 75_000
    | 75_000 -> target := 100_000
    | _ -> failwith "Bad target"
  in

  let curr_level = Level.start_level () in
  let curr_ante = ref 1 in
  let curr_blind = ref "Small" in
  let target = ref 300 in
  let arr =
    ref []
    (* Array.make 72 ("filler" >:: fun _ -> assert_equal 0 1
       ~printer:string_of_int) *)
  in
  let curr_elem = ref 0 in
  let ante = !curr_ante in
  let blind = !curr_blind in
  let level = Level.of_pair (ante, blind) in
  let tgt = !target in
  arr :=
    (* arr.(!curr_elem) <- *)
    ( Level.to_string level ^ " should have correct ante" >:: fun _ ->
      assert_equal ante (Level.ante level) ~printer:string_of_int )
    :: (* arr.(!curr_elem + 1) <- *)
       ( Level.to_string level ^ " should have correct blind" >:: fun _ ->
         assert_equal blind (Level.blind level) ~printer:Fun.id )
    :: (* arr.(!curr_elem + 2) <- *)
       ( Level.to_string level ^ " should have correct target score" >:: fun _ ->
         assert_equal tgt (Level.target_score level) ~printer:string_of_int )
    :: !arr;
  curr_elem := !curr_elem + 3;

  for i = 1 to 23 do
    Level.incr_level curr_level;
    incr_blind curr_blind;
    incr_target target;
    if i mod 3 = 0 then incr curr_ante;
    (* arr.(!curr_elem) <- *)
    let ante = !curr_ante in
    let blind = !curr_blind in
    let level = Level.of_pair (ante, blind) in
    let tgt = !target in
    arr :=
      ( Level.to_string level ^ " should have correct ante" >:: fun _ ->
        assert_equal ante (Level.ante level) ~printer:string_of_int )
      :: (* arr.(!curr_elem + 1) <- *)
         ( Level.to_string level ^ " should have correct blind" >:: fun _ ->
           assert_equal blind (Level.blind level) ~printer:Fun.id )
      :: (* arr.(!curr_elem + 2) <- *)
         ( Level.to_string level ^ " should have correct target score"
         >:: fun _ ->
           assert_equal tgt (Level.target_score level) ~printer:string_of_int )
      :: !arr;
    curr_elem := !curr_elem + 3
    (* print_endline ("CURRENT LEVEL: " ^ Level.to_string curr_level ^ "
       CURRELEM: " ^ string_of_int !curr_elem) *)
  done;
  !arr

let tests =
  "tests"
  >::: List.flatten
         [
           card_tests;
           deck_tests;
           hand_tests;
           basic_scoring_tests;
           joker_scoring_tests;
           end_of_round_tests;
           play_disc_add_tests;
           pay_tests;
           planet_card_str_tests;
           planet_card_effect_tests;
           planet_of_hand_tests;
           level_tests;
         ]

let _ = run_test_tt_main tests
