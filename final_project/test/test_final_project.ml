open OUnit2
open Final_project

let tests =
  "tests"
  >::: [
         ( "simple card test number" >:: fun _ ->
           assert_equal (Card.number (Card.of_pair ("Spades", 5))) 5 );
         ( "simple card test suit" >:: fun _ ->
           assert_equal (Card.suit (Card.of_pair ("Spades", 5))) "Spades" );
         ( "simple card test raise bad suit" >:: fun _ ->
           assert_raises (Invalid_argument "bad suit") (fun _ ->
               Card.of_pair ("Spade", 5)) );
         ( "simple card test raise bad number" >:: fun _ ->
           assert_raises (Invalid_argument "bad int") (fun _ ->
               Card.of_pair ("Spades", 0)) );
         ( "simple card test raise bad suit and bad number" >:: fun _ ->
           assert_raises (Invalid_argument "bad int") (fun _ ->
               Card.of_pair ("Spade", 0)) );
         ( " " >:: fun _ ->
           assert_equal
             (Card.to_string (Card.of_pair ("Spades", 5)))
             "Five of Spades" );
       ]

let _ = run_test_tt_main tests
