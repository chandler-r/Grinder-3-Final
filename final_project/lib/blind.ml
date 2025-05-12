type t =
  | Small
  | Big
  | Boss
  | Defeated

let current_blind = ref Small
let none_blind_threshold = ref 0
let small_blind_threshold = ref 1
let big_blind_threshold = ref 2
let boss_blind_threshold = ref 3

let blind_to_str = function
  | Small -> "Small"
  | Big -> "Big"
  | Boss -> "Boss"
  | Defeated -> "Boss Blind Defeated"

let rec choose_blind () =
  if !current_blind = Defeated then (
    small_blind_threshold :=
      int_of_float (float_of_int !small_blind_threshold ** 1.1);
    big_blind_threshold :=
      int_of_float (float_of_int !big_blind_threshold ** 1.1);
    boss_blind_threshold :=
      int_of_float (float_of_int !boss_blind_threshold ** 1.1);
    current_blind := Small);
  print_endline "\nChoose your blind for this round";
  Printf.printf "\n1. Small Blind (Threshold: %d) \n" !small_blind_threshold;
  Printf.printf "2. Big Blind (Threshold: %d) \n" !big_blind_threshold;
  Printf.printf "3. Boss Blind (Threshold: %d) \n" !boss_blind_threshold;
  Printf.printf "\nYou can only choose the next lowest blind. Next Blind: %s\n"
    (blind_to_str !current_blind);
  print_endline "\nEnter the number of your blind:\n";
  let input = read_line () in
  match input with
  | "1" when !current_blind = Small ->
      current_blind := Big;
      !small_blind_threshold
  | "2" when !current_blind = Big ->
      current_blind := Boss;
      !big_blind_threshold
  | "3" when !current_blind = Boss ->
      current_blind := Defeated;
      !boss_blind_threshold
  | _ ->
      print_endline "\nInvalid choice or blind not available. Try again.\n";
      choose_blind ()
