(* Question 1 *)

(* TODO: Write a good set of tests for {!q1a_nat_of_int}. *)
let q1a_nat_of_int_tests : (int * nat) list = [
  (*( ((1,1), (2,2)), 2 );*)
  (3, (S (S (S Z))));
]

(* TODO:  Implement {!q1a_nat_of_int} using a tail-recursive helper. *)
let rec q1a_nat_of_int (n : int) : nat = 
  let rec helper n acc = 
    if n = 0 then acc else
      helper (n-1) (S acc) 
  in helper n Z

(* TODO: Write a good set of tests for {!q1b_int_of_nat}. *)
let q1b_int_of_nat_tests : (nat * int) list = [
  (Z, 0);
]

(* TODO:  Implement {!q1b_int_of_nat} using a tail-recursive helper. *)
let rec q1b_int_of_nat (n : nat) : int = 
  let rec helper n acc = 
    match n with
    | Z -> 0
    | S more -> (acc + 1) + helper more acc
  in helper n 0

(* TODO: Write a good set of tests for {!q1c_add}. *)
let q1c_add_tests : ((nat * nat) * nat) list = [
  ((S(Z), Z ),S(Z)) ;
]

(* TODO: Implement {!q1c_add}. *)
let rec q1c_add (n : nat) (m : nat) : nat = 
  match n with 
  | Z -> m
  | S more -> q1c_add (more) (S m)


(* Question 2 *)

(* TODO: Implement {!q2a_neg}. *)
let q2a_neg (e : exp) : exp = 
  Times( 
    Const (-1.0), e)

(* TODO: Implement {!q2b_minus}. *)
let q2b_minus (e1 : exp) (e2 : exp) : exp = 
  Plus(e1 , q2a_neg (e2))

(* TODO: Implement {!q2c_pow}. *)
let q2c_pow (e1 : exp) (p : nat) : exp = 
  let rec helper e p =
    match p with
    |Z -> Const(1.0)
    |S more -> Times (e , (helper e more))
  in
  helper e1 p
      


(* Question 3 *)

(* TODO: Write a good set of tests for {!eval}. *)
let eval_tests : ((float * exp) * float) list = [
  (((0.243145005426788963), (Times (Var, Var))) , 0.0591194936639932375) ;
  (((0.0), (Times (Const(0.5), Plus(Var, Var)))) , 0.0) ;
  ((0. , Plus(Const(1.), Var)), 1.);
  ((2. , Div (Const(1.), Var)), 0.5);
  ((1. , Plus(Times(Const(2.) , Var),Var)) , 3.); 

]

(* TODO: Implement {!eval}. *)
let rec eval (a : float) (e : exp) : float = 
  match e with 
  | Var -> a 
  | Const b -> b 
  | Plus (e1, e2)  -> (eval a e1) +. (eval a e2)
  | Times (e1, e2) -> (eval a e1) *. (eval a e2)
  | Div (e1, e2) -> (eval a e1) /. (eval a e2)
           


(* Question 4 *)

(* TODO: Write a good set of tests for {!diff_tests}. *)
let diff_tests : (exp * exp) list = [
  ((Div (Const (1.), Const (1.))), (Div (Plus (Times (Const 0., Const 1.), Times (Times (Const (1.), Const 0.), Const (-1.))), Times (Const 1., Const 1.))));
  ((Times(Var, Var)), ((Plus (Times (Const 1., Var), Times (Var, Const 1.)))));
  ((Var), Const(1.));
  ((Plus(Times(Plus(Var,Var), Var), (Div (Const (1.), Var))), (Plus
                                                                 (Plus (Times (Plus (Const 1., Const 1.), Var),
                                                                        Times (Plus (Var, Var), Const 1.)),
                                                                  Div
                                                                    (Plus (Times (Const 0., Var),
                                                                           Times (Times (Const 1., Const 1.), Const (-1.))),
                                                                     Times (Var, Var))))));
]

(* TODO: Implement {!diff}. *)
let rec diff (e : exp) : exp = 
  match e with 
  | Const a -> Const (0.)
  | Var -> Const (1.)
  | Plus (e1, e2) -> Plus((diff e1), (diff e2))
  | Times (e1, e2) -> Plus((Times((diff e1), e2)),(Times(e1, (diff e2)))) 
  | Div (e1, e2) -> Div(Plus(Times((diff e1), e2), Times(Times(e1, diff(e2)), Const(-1.))), Times(e2,e2))
    
                      

