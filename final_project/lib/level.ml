type ante = int
(* AF: [n] represents Ante #n. *)
(* RI: Must be between 1 and 8 (inclusive). *)

type blind =
  | Small
  | Big
  | Boss
(* AF: [Small] represents a small bind, [Big] represents a big bind, and [Boss]
   represents a boss bind. Smaller binds have lower difficulty and bonuses than
   larger binds. [Small] < [Big] < [Boss]. *)
(* RI: None *)

type level = (ante * blind) ref
(* AF: [(a, b)] represents blind [b] of ante [a]. *)

exception GameOver

let start_level () = ref (1, Small)

let incr_level level =
  match !level with
  | a, Small -> level := (a, Big)
  | a, Big -> level := (a, Boss)
  | a, Boss -> if a < 8 then level := (a + 1, Small) else raise GameOver

let target_score level =
  let a, b = !level in
  let m =
    match b with
    | Small -> 1.
    | Big -> 1.5
    | Boss -> 2.
  in

  let v =
    match a with
    | 1 -> 300
    | 2 -> 800
    | 3 -> 2_000
    | 4 -> 5_000
    | 5 -> 11_000
    | 6 -> 20_000
    | 7 -> 35_000
    | 8 -> 50_000
    | _ -> failwith "Ante must be between 1 and 8"
  in

  (* will always be something point zero, so no rounding error *)
  int_of_float (m *. float_of_int v)

let end_of_round_bonus level =
  match !level with
  | _, Small -> 3
  | _, Big -> 4
  | _, Boss -> 5

let ante level = fst !level

let blind level =
  match !level with
  | _, Small -> "Small"
  | _, Big -> "Big"
  | _, Boss -> "Boss"

let to_string level =
  "Level " ^ string_of_int (ante level) ^ ", " ^ blind level ^ " Blind"

(* let rec choose_next_blind level = if blind level = "Boss" then incr_level
   level; *)
(* small_blind_threshold := (*int_of_float*) (*float_of_int*)
     !small_blind_threshold ** 1.1; big_blind_threshold := (*int_of_float*)
     (*float_of_int*) !big_blind_threshold ** 1.1; boss_blind_threshold :=
     (*int_of_float*) (*float_of_int*) !boss_blind_threshold ** 1.1;
     current_blind := Small); *)
(* print_endline "\nChoose your blind for this round"; Printf.printf "\n1.
     Small Blind (Threshold: %F) \n" !small_blind_threshold; Printf.printf "2.
     Big Blind (Threshold: %F) \n" !big_blind_threshold; Printf.printf "3. Boss
     Blind (Threshold: %F) \n" !boss_blind_threshold; Printf.printf "\nYou can
     only choose the next lowest blind. Next Blind: %s\n" (blind_to_str
     !current_blind); print_endline "\nEnter the number of your blind:\n"; let
     input = read_line () in match input with | "1" when !current_blind = Small
     -> current_blind := Big; !small_blind_threshold | "2" when !current_blind =
     Big -> current_blind := Boss; !big_blind_threshold | "3" when
     !current_blind = Boss -> current_blind := Defeated; !boss_blind_threshold |
     _ -> print_endline "\nInvalid choice or blind not available. Try again.\n";
     choose_blind () *)
(* Printf.printf "Your current blind: %S" (to_string level) *)
