(* open Card *)

type t = Card.t array

exception NotInDeck

(** Abstract Function (AF): A deck [d] is represented as an array of cards of
    type [Card.t]. The deck is viewed as an unordered collection of cards and
    may containt any number of cards (including duplicates). *)

let init () : t =
  let suits = [ "Spades"; "Hearts"; "Diamonds"; "Clubs" ] in
  let ranks = [ 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14 ] in
  Array.of_list
    (List.concat
       (List.map (fun s -> List.map (fun r -> Card.of_pair (s, r)) ranks) suits))

let add_card deck (card : Card.t) = Array.append deck [| card |]
let add_modifier deck card = failwith "Not implemented"

let remove_card deck card =
  let lst = Array.to_list deck in
  (* Helper function to remove the first instance of card. *)
  let rec remove = function
    | [] -> raise NotInDeck
    | h :: t -> if h = card then t else h :: remove t
  in
  Array.of_list (remove lst)

let draw_cards deck n : Card.t list =
  let deck_copy = Array.copy deck in
  let current_length = ref (Array.length deck_copy) in
  let drawn_cards = ref [] in
  for _ = 1 to n do
    if !current_length = 0 then failwith "Not enough cards in the deck";
    let r = Random.int !current_length in
    let card = deck_copy.(r) in
    drawn_cards := card :: !drawn_cards;
    deck_copy.(r) <- deck_copy.(!current_length - 1);
    current_length := !current_length - 1
  done;
  List.rev !drawn_cards

let copy_deck deck : Card.t array = Array.copy deck
let to_list deck = Array.to_list deck
