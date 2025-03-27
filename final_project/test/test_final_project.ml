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
    (Scoring.score_played_cards cards)
    ~printer:string_of_int

let tests =
  "tests"
  >::: [
         ( "simple card test number" >:: fun _ ->
           assert_equal
             (Card.number (Card.of_pair ("Spades", 5)))
             5 ~printer:string_of_int );
         ( "simple card test suit" >:: fun _ ->
           assert_equal
             (Card.suit (Card.of_pair ("Spades", 5)))
             "Spades" ~printer:Fun.id );
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
             "Five of Spades" ~printer:Fun.id );
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

let _ = run_test_tt_main tests
