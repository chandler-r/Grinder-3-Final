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

type level = ante * blind
(* AF: [(a, b)] represents blind [b] of ante [a]. *)

let target_score (a, b) =
  let m =
    match b with
    | Small -> 1.
    | Big -> 1.5
    | Boss -> 2.
  in

  let v =
    match a with
    | 1 -> 300.
    | 2 -> 800.
    | 3 -> 2_000.
    | 4 -> 5_000.
    | 5 -> 11_000.
    | 6 -> 20_000.
    | 7 -> 35_000.
    | 8 -> 50_000.
    | _ -> failwith "Ante must be between 1 and 8"
  in

  int_of_float (m *. v)

let end_of_round_bonus (_, b) =
  match b with
  | Small -> 3
  | Big -> 4
  | Boss -> 5

let ante (a, _) = a

let blind (_, b) =
  match b with
  | Small -> "Small"
  | Big -> "Big"
  | Boss -> "Boss"
