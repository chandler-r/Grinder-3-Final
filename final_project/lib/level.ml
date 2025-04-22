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
