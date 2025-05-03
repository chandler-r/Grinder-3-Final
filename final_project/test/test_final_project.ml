open OUnit2
open Final_project

let card_list_printer cards =
  if List.length cards = 0 then "None"
  else
    List.fold_left
      (fun str card -> str ^ " " ^ Card.to_string card ^ ";")
      "" cards
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
    (Scoring.score_played_cards cards [||])
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
    ( "simple card test raise bad number" >:: fun _ ->
      assert_raises
        (Invalid_argument "Rank must be between 1 and 14 (inclusive).")
        (fun _ -> Card.of_pair ("Spades", 0)) );
    ( "simple card test raise bad suit and bad number" >:: fun _ ->
      assert_raises
        (Invalid_argument "Rank must be between 1 and 14 (inclusive).")
        (fun _ -> Card.of_pair ("Spade", 0)) );
    ( "simple card to_string and of_pair test" >:: fun _ ->
      assert_equal
        (Card.to_string (Card.of_pair ("Spades", 5)))
        "Five of ♠️" ~printer:Fun.id );
  ]

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
  ]

let scoring_tests =
  [
    (* Used this website to calculate played hand scores:
       https://efhiii.github.io/balatro-calculator/?h=AACgAJGAoEQIACA*)
    create_basic_scoring_test 16
      [
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Hearts", 7);
        Card.of_pair ("Clubs", 6);
        Card.of_pair ("Hearts", 4);
        Card.of_pair ("Diamonds", 2);
      ];
    create_basic_scoring_test 48
      [
        Card.of_pair ("Diamonds", 7);
        Card.of_pair ("Hearts", 7);
        Card.of_pair ("Clubs", 6);
        Card.of_pair ("Hearts", 4);
        Card.of_pair ("Diamonds", 2);
      ];
    create_basic_scoring_test 84
      [
        Card.of_pair ("Diamonds", 7);
        Card.of_pair ("Hearts", 7);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Hearts", 4);
        Card.of_pair ("Diamonds", 2);
      ];
    create_basic_scoring_test 153
      [
        Card.of_pair ("Diamonds", 7);
        Card.of_pair ("Hearts", 7);
        Card.of_pair ("Clubs", 6);
        Card.of_pair ("Hearts", 7);
        Card.of_pair ("Diamonds", 2);
      ];
    create_basic_scoring_test 153
      [
        Card.of_pair ("Diamonds", 7);
        Card.of_pair ("Hearts", 7);
        Card.of_pair ("Clubs", 6);
        Card.of_pair ("Spades", 7);
        Card.of_pair ("Diamonds", 2);
      ];
    create_basic_scoring_test 200
      [
        Card.of_pair ("Diamonds", 2);
        Card.of_pair ("Hearts", 3);
        Card.of_pair ("Clubs", 6);
        Card.of_pair ("Hearts", 5);
        Card.of_pair ("Diamonds", 4);
      ];
    create_basic_scoring_test 220
      [
        Card.of_pair ("Diamonds", 2);
        Card.of_pair ("Hearts", 3);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Hearts", 5);
        Card.of_pair ("Diamonds", 4);
      ];
    create_basic_scoring_test 16
      [
        Card.of_pair ("Diamonds", 2);
        Card.of_pair ("Hearts", 3);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Hearts", 6);
        Card.of_pair ("Diamonds", 4);
      ];
    create_basic_scoring_test 244
      [
        Card.of_pair ("Hearts", 2);
        Card.of_pair ("Hearts", 3);
        Card.of_pair ("Hearts", 14);
        Card.of_pair ("Hearts", 6);
        Card.of_pair ("Hearts", 4);
      ];
    create_basic_scoring_test 13
      [
        Card.of_pair ("Diamonds", 2);
        Card.of_pair ("Hearts", 3);
        Card.of_pair ("Hearts", 8);
        Card.of_pair ("Hearts", 6);
        Card.of_pair ("Hearts", 4);
      ];
    create_basic_scoring_test 264
      [
        Card.of_pair ("Diamonds", 2);
        Card.of_pair ("Diamonds", 3);
        Card.of_pair ("Diamonds", 14);
        Card.of_pair ("Diamonds", 14);
        Card.of_pair ("Diamonds", 4);
      ];
    create_basic_scoring_test 272
      [
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 3);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Clubs", 4);
      ];
    create_basic_scoring_test 300
      [
        Card.of_pair ("Spades", 14);
        Card.of_pair ("Spades", 3);
        Card.of_pair ("Spades", 14);
        Card.of_pair ("Spades", 14);
        Card.of_pair ("Spades", 4);
      ];
    create_basic_scoring_test 296
      [
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Diamonds", 14);
        Card.of_pair ("Clubs", 4);
      ];
    create_basic_scoring_test 728
      [
        Card.of_pair ("Diamonds", 14);
        Card.of_pair ("Spades", 14);
        Card.of_pair ("Spades", 14);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Hearts", 4);
      ];
    create_basic_scoring_test 1000
      [
        Card.of_pair ("Spades", 14);
        Card.of_pair ("Spades", 2);
        Card.of_pair ("Spades", 5);
        Card.of_pair ("Spades", 3);
        Card.of_pair ("Spades", 4);
      ];
    create_basic_scoring_test 1680
      [
        Card.of_pair ("Diamonds", 4);
        Card.of_pair ("Spades", 4);
        Card.of_pair ("Spades", 4);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Hearts", 4);
      ];
    create_basic_scoring_test 2436
      [
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Clubs", 14);
        Card.of_pair ("Clubs", 4);
      ];
    create_basic_scoring_test 2880
      [
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 4);
        Card.of_pair ("Clubs", 4);
      ];
  ]

let dummy_deck = Deck.init ()
let dummy_hand = Hand.to_hand (Deck.draw_cards dummy_deck 5)

let rec pick_n n hand =
  let card_list = Hand.to_list hand in
  match n with
  | 0 -> []
  | _ -> List.nth card_list (n - 1) :: pick_n (n - 1) hand

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

let play_disc_tests =
  List.map QCheck_runner.to_ounit2_test [ play_test; discard_test ]

let pay_check amount =
  Money.money := 4;
  if amount <= 4 then (
    Money.pay amount;
    !Money.money = 4 - amount)
  else true

let pay_test = QCheck.(Test.make ~count:10 ~name:"pay" small_nat pay_check)

let overdraw_check amount =
  Money.money := 4;
  if amount > 4 then
    try
      Money.pay amount;
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
  Money.money := starting_cash;
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
  Money.end_of_round level 0;
  assert_equal expected_output !Money.money ~printer:string_of_int

let end_of_round_tests =
  [
    end_of_round_test (0, 5, 9);
    end_of_round_test (0, 4, 7);
    end_of_round_test (3, 4, 7);
    end_of_round_test (1, 4, 8);
    end_of_round_test (2, 4, 9);
    end_of_round_test (0, 30, 38);
  ]

let tests =
  "tests"
  >::: List.flatten
         [
           card_tests; deck_tests; hand_tests; scoring_tests; end_of_round_tests;
         ]
       @ play_disc_tests @ pay_tests

let _ = run_test_tt_main tests
