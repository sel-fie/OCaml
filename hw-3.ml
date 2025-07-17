(* Hi everyone. All of these problems are generally "one-liners" and have slick solutions. They're quite cute to think
   about but are certainly confusing without the appropriate time and experience that you devote towards reasoning about
   this style. Good luck! :-) *)

(* For example, if you wanted to use the encoding of five in your test cases, you could define: *)
let five : 'b church = fun s z -> s (s (s (s (s z))))
(* and use 'five' like a constant. You could also just use
   'fun z s -> s (s (s (s (s z))))' directly in the test cases too. *)

(* If you define a personal helper function like int_to_church, use it for your test cases, and see things break, you should
   suspect it and consider hard coding the input cases instead *)

(*---------------------------------------------------------------*)
(* QUESTION 1 *)

(* Question 1a: Church numeral to integer *)
(* TODO: Test cases *)
let to_int_tests : (int church * int) list = [
  (zero, 0);
  (one, 1);
  (five, 5);
  
] 
(* TODO: Implement:
   Although the input n is of type int church, please do not be confused. This is due to typechecking reasons, and for
   your purposes, you could pretend n is of type 'b church just like in the other problems. *)
let to_int (n : int church) : int = 
  n (fun x -> x + 1) 0

(* Question 1b: Determine if a church numeral is zero *)
(* TODO: Test cases *)
let is_zero_tests : ('b church * bool) list = [
  (zero, true);
  (one, false);
  (five, false);
]

(* TODO: Implement *)
let is_zero (n : 'b church) : bool = 
  n (fun s -> false )true
  
(* Question 1c: Add two church numerals *)
(* TODO: Test cases *)
let add_tests : (('b church * 'b church) * 'b church) list = [
  ((zero, one), one); 
  ((zero, zero), zero);
  (((fun s z -> s (s (s (z)))), (fun s z -> s ( s (z)))), five);
]

(* TODO: Implement *)
let add (n1 : 'b church) (n2 : 'b church) : 'b church = 
  fun f i -> n1 f (n2 f i)
      
  
    (* replace the zero of the first numeral with the second numeral *)
    (* needs to return a function, not the answer of the function *)

(*---------------------------------------------------------------*)
(* QUESTION 2 *)

(* Question 2a: Multiply two church numerals *)
(* TODO: Test cases *)
let mult_tests : (('b church * 'b church) * 'b church) list = [
  ((zero, zero), zero);
  ((zero, one), zero); 
  (((fun s z -> s (s (s (z)))), (fun s z -> s ( s (z)))), (fun s z -> s (s (s(s(s(s(z))))))));
]

(* TODO: Implement *)
let mult (n1 : 'b church) (n2 : 'b church) : 'b church = 
  fun f -> n1 (n2 f)

(* Question 2b: Compute the power of a church numeral given an int as the power *)
(* TODO: Test cases *)
let int_pow_church_tests : ((int * 'b church) * int) list = [
  ((2,(fun s z -> s ( s (z)))), 4);
  ((2, zero), 1);
  ((3, (fun s z -> s (s (s (z))))), 27);
  (( 3,(fun s z -> s ( s (z)))), 9);
  (( 0, zero), 1);
  (( 0, one), 0);

]

(* TODO: Implement *)
let int_pow_church (x : int) (n : 'b church) : int = 
  n (fun f -> f * x) 1
  
  
  