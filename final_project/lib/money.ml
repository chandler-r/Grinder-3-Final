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
  print_endline ("Currently owned: $" ^ string_of_int !curr_money);
  print_endline ("+ $" ^ string_of_int x ^ " (Beat the blind)");
  print_endline ("+ $" ^ string_of_int hands ^ " ($1 per hand remaining)");
  print_endline
    ("+ $"
    ^ string_of_int
        (if interest < !interest_cap then interest else !interest_cap)
    ^ " ($1 interest per $5 currently owned, up to $5)");
  curr_money :=
    !curr_money + x + hands
    + if interest < !interest_cap then interest else !interest_cap
