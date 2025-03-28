type t = Card.t list
(* AF: [card1; card2; ...; cardn] is a representation of a collection of cards, either held in hand or a subset of those to be played or discarded. *)
(* RI: the length of the list representing a hand should be no more than hand_size. The length of the list representing cards played or discarded should be no more than play_limit *)

type hands =
  | HighCard
  | Pair
  | TwoPair
  | ThreeKind
  | Straight
  | Flush
  | FullHouse
  | FourKind
  | StraightFlush
  | FiveKind
  | FlushHouse
  | FlushFive

exception TooManyCards

let hand_size = ref 5
let play_limit = ref 5
let hands_per_round = ref 3

let rep_ok_hand hand =
  if List.length hand <= !hand_size then hand else raise TooManyCards

let rep_ok_play play =
  if List.length play <= !play_limit then play else raise TooManyCards

(** [count_dup_ranks [] card_lst] is an association list matching a rank to the
    number of times a card in [card_lst] has that rank. *)
let rec count_dup_ranks acc (card_lst : Card.t list) =
  match card_lst with
  | [] -> acc
  | h :: t -> (
      match List.assoc_opt (Card.number h) acc with
      | None -> count_dup_ranks ((Card.number h, 1) :: acc) t
      | Some x ->
          count_dup_ranks
            ((Card.number h, x + 1) :: List.remove_assoc (Card.number h) acc)
            t)

(** [is_flush card_lst] outputs [true] if [card_lst] contains a flush (5 cards
    of the same suit), and [false] otherwise. *)
let is_flush card_lst =
  List.length card_lst = 5
  && List.filter
       (fun elem -> Card.suit (List.hd card_lst) = Card.suit elem)
       card_lst
     |> List.length = 5

(** [is_ascending_order 0 card_lst] outputs [true] if the rank of every card in
    [card_lst] is one higher than the rank of the card preceding it in
    [card_lst], and [false] otherwise (an Ace has a rank of 1 or 14). Returns
    [true] if [card_lst] is empty. *)
let rec is_ascending_order prev card_lst =
  match card_lst with
  | [] -> true
  | [ a; b ] ->
      if Card.number b = 14 && Card.number a = 5 then true
        (* If Ace is last card in sorted list, then check if the preceding card
           is a 5; this implies the 3 other cards were 2, 3, 4, and we have a
           straight (else this function would've returned false already) *)
      else if Card.number b = Card.number a + 1 then true
      else false
  | h :: t ->
      if prev = 0 then is_ascending_order (Card.number h) t
      else if Card.number h = prev + 1 then is_ascending_order (Card.number h) t
      else false

(** [is_straight card_lst] outputs [true] if [card_lst] contains a straight, and
    [false] otherwise. *)
let is_straight card_lst =
  if List.length card_lst != 5 then false
  else
    let sorted_lst =
      List.sort
        (fun card1 card2 -> Card.number card1 - Card.number card2)
        card_lst
    in
    is_ascending_order 0 sorted_lst

let highest_hand played =
  let played_hand = rep_ok_play played in
  let dups_lst =
    List.sort
      (fun (a, b) (c, d) -> if b - d = 0 then c - a else d - b)
      (count_dup_ranks [] played_hand)
    (* If cards have the same frequency in the hand, then the higher-ranked card
       shows first*)
  in
  let is_flush = is_flush played_hand in
  let is_straight = is_straight played_hand in
  let highest_dups = List.hd dups_lst in
  let snd_highest_dups =
    if List.length dups_lst > 1 then List.hd (List.tl dups_lst) else (0, 0)
  in
  if is_flush && is_straight then (StraightFlush, played_hand)
    (* Straight flush *)
  else
    match highest_dups with
    | rank, 5 ->
        if is_flush then (FlushFive, played_hand) (* Flush five *)
        else (FiveKind, played_hand) (* Five of a kind *)
    | rank, 4 ->
        (FourKind, List.filter (fun card -> Card.number card = rank) played_hand)
        (* Four of a kind *)
    | rank, 3 -> (
        match snd_highest_dups with
        | snd_rank, 2 ->
            if is_flush then (FlushHouse, played_hand) (* Flush house *)
            else
              ( FullHouse,
                List.filter (fun card -> Card.number card = rank) played_hand
                @ List.filter
                    (fun card -> Card.number card = snd_rank)
                    played_hand ) (* Full House *)
        | snd_rank, _ ->
            (* Flush > Straight > Three of a kind*)
            if is_flush then (Flush, played_hand)
            else if is_straight then (Straight, played_hand)
            else
              ( ThreeKind,
                List.filter (fun card -> Card.number card = rank) played_hand ))
    | rank, 2 -> (
        match snd_highest_dups with
        | snd_rank, 2 ->
            (* Flush > Straight > Two pair *)
            if is_flush then (Flush, played_hand)
            else if is_straight then (Straight, played_hand)
            else
              ( TwoPair,
                List.filter (fun card -> Card.number card = rank) played_hand
                @ List.filter
                    (fun card -> Card.number card = snd_rank)
                    played_hand )
        | snd_rank, _ ->
            (* Flush > Straight > Pair *)
            if is_flush then (Flush, played_hand)
            else if is_straight then (Straight, played_hand)
            else
              ( Pair,
                List.filter (fun card -> Card.number card = rank) played_hand ))
    | _ ->
        (* Flush > Straight > High card*)
        if is_flush then (Flush, played_hand)
        else if is_straight then (Straight, played_hand)
        else
          ( HighCard,
            [
              List.sort (fun a b -> Card.number b - Card.number a) played_hand
              |> List.hd;
            ] )

let played_hand_type hand =
  match hand with
  | HighCard -> "high card"
  | Pair -> "pair"
  | TwoPair -> "two pair"
  | ThreeKind -> "three of a kind"
  | Straight -> "straight"
  | Flush -> "flush"
  | FullHouse -> "full house"
  | FourKind -> "four of a kind"
  | StraightFlush -> "straight flush"
  | FiveKind -> "five of a kind"
  | FlushHouse -> "flush house"
  | FlushFive -> "flush five"

let cycle_cards cards hand =
  (* let len = List.length cards in *)
  List.filter (Fun.negate (fun y -> List.mem y cards)) hand
(*@ Deck.draw len*)
(*TODO: will need to use some Deck.draw function where we draw [len] cards if
  possible*)

let discard cards hand =
  let x = rep_ok_hand cards in
  cycle_cards x hand

let play cards hand =
  let x = rep_ok_play cards in
  (cycle_cards x hand, highest_hand cards)

let to_list cards = cards
