let money = ref 4

(* type blind = | Small | Big | Boss *)

exception InsufficientFunds

let interest_cap = ref 5
let pay x = if !money >= x then money := !money - x else raise InsufficientFunds

let end_of_round level hands =
  let x = Level.end_of_round_bonus level in
  money := !money + x + hands;
  let interest = !money / 5 in
  if interest < !interest_cap then money := !money + interest
  else money := !money + !interest_cap
