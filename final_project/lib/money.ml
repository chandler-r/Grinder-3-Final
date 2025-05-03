let money = ref 4

exception InsufficientFunds

let interest_cap = ref 5
let pay x = if !money >= x then money := !money - x else raise InsufficientFunds

let end_of_round level hands =
  let x = Level.end_of_round_bonus level in
  let interest = !money / 5 in
  money := !money + x + hands;
  if interest < !interest_cap then money := !money + interest
  else money := !money + !interest_cap
