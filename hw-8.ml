(** Part 1: Parsing *)
let parse_exp_tests : (string * exp option) list = [
  ("", None)
]

let rec exp_parser i =
  let open Parser in
  (** Use [identifier] and [keyword] in your implementation,
      not [identifier_except] and [keyword_among] directly *)
  let identifier, keyword =
    let keywords = ["true"; "false"; "let"; "in"; "end"; "if"; "then"; "else"; "fn"; "rec"] in
    identifier_except keywords, keyword_among keywords
  in
  (* start from atomic_exp *)
  let atomic_exp_parser = 
    first_of [ const_map (ConstB true) (keyword "true");
               const_map (ConstB false) (keyword "false");
               map (fun x -> ConstI x) (int_digits);
               map (fun y -> Var y) (identifier);
               (symbol "(" |*> fun _ -> 
                   exp_parser |*> fun e ->
                     symbol ")" |*> fun _ -> of_value e); 
             ]
  in
  let applicative_exp_parser = 
    left_assoc_op (spaces)(atomic_exp_parser)(fun e1 _ e2 -> Apply(e1, e2)) 
  in
  let negatable_exp_parser = 
    first_of [ 
      map3 (fun (w,x) (y,z) _ -> LetComma (w,x,y,z)) (map2 (fun x y -> (x,y)) (keyword "let" |>> symbol "(" |>> identifier)
                                                        (symbol "," |>> identifier)) (map2 (fun x y -> (x,y)) (symbol ")" |>> symbol "=" |>> exp_parser) 
                                                                                        (keyword "in" |>> exp_parser)) (keyword "end");
      map3 (fun x y (z,_) -> Let (x, y ,z)) (keyword "let" |>> identifier) 
        (symbol "=" |>> exp_parser) (map2 (fun x y -> (x,y))(keyword "in" |>> exp_parser)(keyword "end"));
      map3 (fun x y z -> If (x,y,z)) (keyword "if" |>> exp_parser) 
        (keyword "then" |>> exp_parser) (keyword "else" |>> exp_parser);
      map3 (fun x y z -> Fn (x,y,z)) (keyword "fn" |>> identifier)
        (optional(symbol ":") |>> optional(typ_parser)) (symbol "=>" |>> exp_parser);
      map3 (fun x y z -> Rec (x,y,z)) (keyword "rec" |>> identifier)
        (optional(symbol ":") |>> optional(typ_parser)) (symbol "=>" |>> exp_parser);
      applicative_exp_parser 
    ] 
  in
  let negation_exp_parser = 
    prefix_op (symbol "-") (negatable_exp_parser)(fun _ e -> PrimUop(Negate,e))
  in
  let multiplicative_exp_parser = 
    left_assoc_op (symbol "*") (negation_exp_parser)
      (fun left _ right -> PrimBop(left, Times, right))
  in 
  let additive_exp_parser =
    left_assoc_op (first_of [((const_map (Plus) (symbol "+"))) ;(const_map (Minus)(symbol "-"))])
      (multiplicative_exp_parser) (fun left op right -> PrimBop(left, op, right)) 
  in
  let comparative_exp_parser =
    non_assoc_op (first_of_2 ((const_map (Equals) (symbol "="))) (const_map (LessThan)(symbol "<")))
      (additive_exp_parser) (fun left op right -> PrimBop(left, op, right))
  in
  (** You may need to define helper parsers depending on [exp_parser] here *)
  let exp_parser_impl =
    non_assoc_op (symbol ",") comparative_exp_parser 
      (fun left _ right -> Comma(left, right))
  in
  exp_parser_impl i

(** DO NOT Change This Definition *)
let parse_exp : string -> exp option =
  let open Parser in
  run (between spaces eof exp_parser)
