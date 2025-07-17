(** Part 3: Type Inference *)
let typ_infer_test_helper_tests : ((Context.t * exp) * typ option) list = [
  ((Context.empty, ConstB true), Some Bool)
]

let rec typ_infer (ctx : Context.t) (e : exp) : typ =
  match e with
  | ConstI _ -> Int
  | PrimBop (e1, bop, e2) ->
      let ((t1, t2), t3) = bop_type bop in
      if typ_infer ctx e1 = t1 && typ_infer ctx e2 = t2
      then t3
      else raise TypeInferenceError
  | PrimUop (uop, e') ->
      let (t1, t2) = uop_type uop in
      if typ_infer ctx e' = t1
      then t2
      else raise TypeInferenceError

  | ConstB _ -> Bool
  | If (e', e1, e2) -> 
      if typ_infer ctx e' <> Bool then raise TypeInferenceError; 
      let t = typ_infer ctx e1 in
      if t <> typ_infer ctx e2 then raise TypeInferenceError;
      t 
  | Comma (e1, e2) -> Pair (typ_infer ctx e1, typ_infer ctx e2)
  | LetComma (x, y, e1, e2) -> 
      begin
        match typ_infer ctx e1 with
        | Pair (w,z) -> 
            let ct = Context.extend ctx (x,w) in
            let c = Context.extend ct (y, z) in
            typ_infer c e2 
        | _ -> raise TypeInferenceError
      end

  | Fn (x, Some t, e') -> Arrow (t, typ_infer (Context.extend ctx (x,t)) e') 
               
  | Apply (e1, e2) -> begin
      let t = typ_infer ctx e1 in
      let t2 =  typ_infer ctx e2 in
      match t with 
      | Arrow (a, b) -> if a = t2 then b else 
            raise TypeInferenceError
      | _ -> raise TypeInferenceError 
    end 
  | Rec (f, Some t, e') -> let x = typ_infer (Context.extend ctx (f,t)) e' 
      in if x = t then t else raise TypeInferenceError
  | Let (x, e1, e2) -> 
      let t = typ_infer ctx e1 in 
      typ_infer (Context.extend ctx (x,t)) e2
  | Var x ->
      begin
        match Context.lookup ctx x with
        | Some t -> t
        | None -> raise TypeInferenceError
      end 
  (** You can ignore these cases for Part 2 *)
  | Fn (_, None, _) -> raise IgnoredInPart3
  | Rec (_, None, _) -> raise IgnoredInPart3

(** DO NOT Change This Definition *)
let typ_infer_test_helper ctx e =
  try
    Some (typ_infer ctx e)
  with
  | TypeInferenceError -> None

(** Part 4: Unification & Advanced Type Inference *)
let unify_test_case1 () =
  let x = new_tvar () in
  let y = new_tvar () in
  y := Some Int;
  (TVar x, TVar y)

let unify_test_case2 () =
  let x = new_tvar () in
  (TVar x, Arrow (TVar x, TVar x))

let unify_test_helper_tests : ((unit -> typ * typ) * bool) list = [
  ((fun () -> (Int, Int)), true);
  ((fun () -> (Int, Bool)), false);
  (unify_test_case1, true);
  (unify_test_case2, false)
]

let rec unify : typ -> typ -> unit =
  let rec occurs_check (x : typ option ref) (t : typ) : bool =
    let t = rec_follow_tvar t in
    match t with
    | Int -> false
    | Bool -> false
    | Pair (t1, t2) -> occurs_check x t1 || occurs_check x t2
    | Arrow (t1, t2) -> occurs_check x t1 || occurs_check x t2
    | TVar y -> 
        if is_same_tvar x y then true
        else
          ( match !y with
            | None -> false
            | Some t' -> occurs_check x t'
          )
  in
  fun ta tb ->
    let ta = rec_follow_tvar ta in
    let tb = rec_follow_tvar tb in
    match ta, tb with
    | Int, Int -> ()
    | Bool, Bool -> ()
    | Pair (ta1, ta2), Pair (tb1, tb2) -> 
        unify ta1 tb1; unify ta2 tb2
    | Arrow (ta1, ta2), Arrow (tb1, tb2) -> 
        unify ta1 tb1; unify ta2 tb2
    | TVar xa, TVar xb when is_same_tvar xa xb -> ()
    | TVar xa, _ ->
        if occurs_check xa tb then
          raise OccursCheckFailure;
        (match !xa with
         | None -> xa := Some tb
         | Some t -> unify t tb )
    | _, TVar xb -> unify tb ta
    | _, _ -> raise UnificationFailure

(** DO NOT Change This Definition *)
let unify_test_helper f =
  let ta, tb = f () in
  try
    unify ta tb; true
  with
  | UnificationFailure -> false
  | OccursCheckFailure -> false

let adv_typ_infer_test_case1 =
  let x = new_tvar () in
  ((Context.empty, Fn ("y", None, Var "y")), Some (Arrow (TVar x, TVar x)))

let adv_typ_infer_test_helper_tests : ((Context.t * exp) * typ option) list = [
  adv_typ_infer_test_case1
]

let rec adv_typ_infer (ctx : Context.t) (e : exp) : typ =
  match e with
  | ConstI n -> Int
  | PrimBop (e1, bop, e2) -> begin
      let ((ex1,ex2), t) = bop_type bop in
      let a1 = adv_typ_infer ctx e1 in
      let a2 = adv_typ_infer ctx e2 in
      unify a1 ex1;
      unify a2 ex2;
      t 
    end
  | PrimUop (uop, e') -> 
      begin
        let (ext, t) = uop_type uop in
        let a = adv_typ_infer ctx e' in
        unify a ext; t
      end

  | ConstB b -> Bool
  | If (e', e1, e2) -> 
      let c = adv_typ_infer ctx e' in
      unify c Bool;
      let t1 = adv_typ_infer ctx e1 in
      let t2 = adv_typ_infer ctx e2 in
      unify t1 t2; t1

  | Comma (e1, e2) -> Pair (adv_typ_infer ctx e1, adv_typ_infer ctx e2)
  | LetComma (x, y, e1, e2) -> 
      (match rec_follow_tvar (adv_typ_infer ctx e1) with
       | Pair (t1,t2) -> let c = Context.extend (Context.extend ctx (x,t1)) (y,t2) in
           adv_typ_infer c e2
       | _ -> raise TypeInferenceError
      )
  | Fn (x, Some t, e') -> let c = Context.extend ctx (x,t) in 
      Arrow (t , adv_typ_infer c e')
  | Fn (x, None, e') -> let a = TVar (ref None) in
      let b = adv_typ_infer (Context.extend ctx (x,a)) e' in 
      Arrow (a,b) 
  | Apply (e1, e2) -> 
      begin
        let a = adv_typ_infer ctx e1 in
        let b = adv_typ_infer ctx e2 in
        match rec_follow_tvar a with
        | Arrow (x,y) ->
            unify x b ; y
        | _ -> raise TypeInferenceError
      end

  | Rec (f, Some t, e') -> 
      let c = Context.extend ctx (f,t) in
      let t' = adv_typ_infer c e' in
      unify t' t; t
  | Rec (f, None, e') -> 
      let a = TVar (ref None) in
      let b = TVar (ref None) in
      let fn = Arrow (a, b) in 
      let c = Context.extend ctx (f, fn) in
      let t = adv_typ_infer c e' in 
      unify fn t; 
      rec_follow_tvar fn 
  | Let (x, e1, e2) -> 
      let t = adv_typ_infer ctx e1 in
      adv_typ_infer (Context.extend ctx (x, t)) e2
  | Var x -> match Context.lookup ctx x with
    | Some t -> t
    | None -> raise TypeInferenceError

(** DO NOT Change This Definition *)
let adv_typ_infer_test_helper ctx e =
  try
    Some (adv_typ_infer ctx e)
  with
  | UnificationFailure -> None
  | OccursCheckFailure -> None
  | TypeInferenceError -> None

(**
 ************************************************************
 You Don't Need to Modify Anything After This Line
 ************************************************************

 Following definitions are the helper entrypoints
 so that you can do some experiments in the top-level.
 Once you implement [exp_parser], [typ_infer], and [eval],
 you can test them with [infer_main] in the top-level.
 Likewise, once you implement [exp_parser], [adv_typ_infer], and [eval],
 you can test them with [adv_infer_main] in the top-level.
 *)
let infer_main exp_str =
  match parse_exp exp_str with
  | None -> raise ParserFailure
  | Some e ->
      print_string "input expression       : "; print_exp e; print_newline ();
      let t = typ_infer Context.empty e in
      print_string "type of the expression : "; print_typ t; print_newline ();
      print_string "evaluation result      : "; print_exp (eval e); print_newline ()

let adv_infer_main exp_str =
  match parse_exp exp_str with
  | None -> raise ParserFailure
  | Some e ->
      print_string "input expression       : "; print_exp e; print_newline ();
      let t = adv_typ_infer Context.empty e in
      print_string "type of the expression : "; print_typ t; print_newline ();
      print_string "evaluation result      : "; print_exp (eval e); print_newline ()
