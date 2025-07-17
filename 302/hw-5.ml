(* TODO: Add test cases for both Question 1 and 2. *)
let xyz_truth_asgn : truth_assignment
  =  Variable_map.empty
     |> Variable_map.add       "x" true
     |> Variable_map.add       "y" false 
       
let truth =
  Variable_map.empty
  |> Variable_map.add "y" false
  |> Variable_map.add "z" false
    
    
let find_sat_assignment_tests : (formula * truth_assignment option) list = [
  (parse_formula "x & ~y ", Some(xyz_truth_asgn));
  (parse_formula "~((z | y))", Some(truth)); 
  (parse_formula "~x & x", None);
  (parse_formula "(~w & (w & w))" , None);
] 

(* Question 1 *)
(*----------------------------------------*)

(* TODO: Implement the function. *)
let find_sat_assignment_exc (formula : formula) : truth_assignment =
  let var = collect_variables formula in
  let rec sat vars map = 
    (* produce truth assignment, then backtrack to see if it works for said function *)
    match vars with 
    | [] -> if eval map formula then map else raise Unsatisfiable_formula
    | x :: xs -> 
        try sat xs (Variable_map.add x true map) with
        | Unsatisfiable_formula -> sat xs (Variable_map.add x false map)
  in sat var Variable_map.empty

(* Question 2 *)
(*----------------------------------------*)

(* TODO: Implement the function. *)
let rec find_sat_assignment_cps (formula : formula)
    (return : truth_assignment -> 'r) (fail : unit -> 'r) : 'r =
  let var = collect_variables formula in
  let rec sat vars map success fail =  
    match vars with
    | [] -> if eval map formula then return map else fail ()
    | x ::xs -> sat xs (Variable_map.add x true map) (success) 
                  (fun () -> sat xs (Variable_map.add x false map) success fail) 
  in sat var Variable_map.empty return fail
    
