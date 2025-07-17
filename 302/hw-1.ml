(* Question 1: Manhattan Distance *)
(* TODO: Write a good set of tests for distance. *)
let distance_tests = [
  ( ((0, 0), (0, 0)), (* input: two inputs, each a pair, so we have a pair of pairs *)
    0                 (* output: the distance between (0,0) and (0,0) is 0 *)
  );                    (* end each case with a semicolon *)
    (* Your test cases go here *)
  ( ((1,1), (2,2)), 2 );
  
  ( ((2,2), (1,1)), 2);
  
  ( ((-1, -1), (-2,-2)), 2);
]
;;

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let distance (x1, y1) (x2, y2) = 
  abs (x2 - x1) + abs (y2 - y1)



(* Question 2: Binomial *)
(* TODO: Write your own tests for the binomial function.
         See the provided test for how to write test cases.
         Remember that we assume that  n >= k >= 0; you should not write test cases where this assumption is violated.
*)
let binomial_tests = [
  (* Your test cases go here. Correct this incorrect test case for the function. *)
  ((0, 0), 1);
  ((1,1), 1);
  ((3,3), 1);
  ((5,0), 1);
  ((10,9),10); 
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let binomial n k = 
  let rec factorial s = if s = 0 then 1 else (s * factorial (s-1)) in
  ((factorial n) / ((factorial k) * (factorial (n-k))))
                  


(* Question 3: Lucas Numbers *)

(* TODO: Write a good set of tests for lucas_tests. *)
let lucas_tests = [
  ((0), 2);
  ((1), 1);
  ((5), 11);
  
]

(* TODO: Implement a tail-recursive helper lucas_helper. *)
let rec lucas_helper n a b= 
  if n = 0 then a else 
    lucas_helper (n-1) b (a+b)
  

(* TODO: Implement lucas by calling lucas_helper. *)
let lucas n = 
  lucas_helper n 2 1
