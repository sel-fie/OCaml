let open_account (pass : password) : bank_account =
  (* TODO: create any helper variables and/or functions *)
  let counter = ref 0 in
  let balance = ref 0 in 
  
  (* TODO: Implement deposit to add money to the account *)
  let deposit p amt = 
    if !counter = 3 then
      raise account_locked
    else if p = pass then 
      (if amt < 0 then raise negative_amount 
       else balance := !balance + amt; counter := 0) 
    else if p != pass then
      (counter := !counter + 1; raise wrong_pass; )
     
  in
  (* TODO: Implement show_balance for the account *)
  let show_balance p = 
    if !counter = 3 then
      raise account_locked
    else if p = pass then
      (counter := 0 ; !balance)
    else (*if p != pass then *) 
      (counter := !counter + 1; raise wrong_pass)
  in
  (* TODO: Implement withdraw money for the account *)
  let withdraw p amt = 
    if !counter = 3 then
      raise account_locked
    else if p = pass then 
      (if amt > !balance then raise not_enough_balance
       else if amt < 0 then raise negative_amount
       else balance := !balance - amt; counter := 0)
    else if p != pass then 
      (counter := !counter + 1; raise wrong_pass; )
    
  in
  {
    deposit;
    show_balance;
    withdraw;
  }