(* SECTION 1 *)

(*  Question 1.1 *)
let rec repeat (x : 'a) : 'a stream = 
  {
    head = x;
    tail = Susp (fun () -> repeat x)
    
  }

(* Question 1.2 *)
let rec filter (f : 'a -> bool) (s : 'a stream) : 'a stream =
  if f s.head then
    { 
      head = s.head;
      tail = Susp (fun () -> filter f (force s.tail)) 
    } 
  else filter f (force s.tail)


(* Question 1.3 *)
let rec lucas1 =
  {
    (* You should fix these *)
    head = 2; 
    tail = Susp (fun () -> lucas2);
  }

and lucas2 =
  {
    (* You should fix these *)
    head = 1;
    tail = Susp (fun () -> zip_with ( + ) lucas1 lucas2);
  }

(* Question 1.4 *)
let unfold (f : 'a -> 'b * 'a) (seed : 'a) : 'b stream =
  map (fun s -> fst (f s)) (iterate (fun s -> snd (f s)) seed)

(* Question 1.5 *)
let unfold_lucas : int stream = 
  unfold (fun (l1, l2) -> (l1 ,(l2, l1+l2))) (2,1)

(* SECTION 2 *)

(* Question 2.1 *)
let rec scale (s1 : int stream) (n : int) : int stream = 
  map (fun e -> e * n) s1

(* Question 2.2 *)
let rec s = 
  {
    head = 1;
    tail = Susp(fun () -> merge (scale s 2) (merge (scale s 3) (scale s 5)))
  
  }
