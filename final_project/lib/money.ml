let money () = ref 4

exception InsufficientFunds

let interest_cap = ref 5

let pay x curr_money =
  if !curr_money >= x then curr_money := !curr_money - x
  else raise InsufficientFunds

let end_of_round level hands curr_money =
  let x = Level.end_of_round_bonus level in
  let interest = !curr_money / 5 in
  (* curr_money := !curr_money + x + hands; if interest < !interest_cap then
     curr_money := !curr_money + interest else *)
  curr_money :=
    !curr_money + x + hands
    + if interest < !interest_cap then interest else !interest_cap
