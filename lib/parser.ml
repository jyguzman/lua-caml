open Token;;

exception InvalidToken of string;;

let raise_invalid_token token extra = 
  let name, lexeme, line, col = token.name, token.lexeme, token.line, token.col in
  let details = String.concat "" [name; ", '"; lexeme; "' line "; string_of_int line; ", col "; string_of_int col] in
  let message = "Invalid token: (" ^ details ^ ") " ^ extra in 
  let exc = InvalidToken message in raise exc

let rec parse_expr expr tokens = 
  parse_and_or expr tokens

  and parse_and_or expr tokens =
    let rec parse_expr_aux left remaining =
      match remaining with 
        | [] -> left, []
        | x :: xs -> 
          let right, rest = parse_comparison Ast.Nil xs in
          match x.token_type with 
            | Keywords And -> parse_expr_aux (Ast.And (left, right)) rest
            | Keywords Or -> parse_expr_aux (Ast.Or (left, right)) rest
            | _ -> left, remaining
    in 
      let left, remaining = parse_comparison expr tokens in 
        parse_expr_aux left remaining
  
  and parse_comparison expr tokens = 
    let rec parse_expr_aux left remaining =
      match remaining with 
        | [] -> left, []
        | x :: xs -> 
          let right, rest = parse_concat Ast.Nil xs in
          match x.token_type with 
            | BinOp Equal -> parse_expr_aux (Ast.Equal (left, right)) rest
            | BinOp Less -> parse_expr_aux (Ast.Less (left, right)) rest
            | BinOp Leq -> parse_expr_aux (Ast.Leq (left, right)) rest
            | BinOp Greater -> parse_expr_aux (Ast.Greater (left, right)) rest
            | BinOp Geq -> parse_expr_aux (Ast.Geq (left, right)) rest
            | BinOp NotEqual -> parse_expr_aux (Ast.Neq (left, right)) rest
            | _ -> left, remaining
    in 
      let left, remaining = parse_concat expr tokens in 
        parse_expr_aux left remaining
    
  and parse_concat expr tokens = 
    let rec parse_expr_aux left remaining =
      match remaining with 
        | [] -> left, []
        | x :: xs -> 
          let right, rest = parse_term Ast.Nil xs in
          match x.token_type with 
            | BinOp DotDot -> parse_expr_aux (Ast.Concat (left, right)) rest
            | _ -> left, remaining
    in 
      let left, remaining = parse_term expr tokens in 
        parse_expr_aux left remaining
  
  and parse_term expr tokens = 
    let rec parse_expr_aux left remaining =
      match remaining with 
        | [] -> left, []
        | x :: xs -> 
          let right, rest = parse_factor Ast.Nil xs in
          match x.token_type with 
            | Minus -> parse_expr_aux (Ast.Subtract (left, right)) rest
            | BinOp Plus -> parse_expr_aux (Ast.Add (left, right)) rest
            | _ -> left, remaining
    in 
      let left, remaining = parse_factor expr tokens in 
        parse_expr_aux left remaining

  and parse_factor expr tokens = 
    let rec parse_expr_aux left remaining =
      match remaining with 
        | [] -> left, []
        | x :: xs -> 
          let right, rest = parse_unary Ast.Nil xs in
          match x.token_type with 
            | BinOp Star -> parse_expr_aux (Ast.Multiply (left, right)) rest
            | BinOp Slash -> parse_expr_aux (Ast.Divide (left, right)) rest
            | _ -> left, remaining
    in 
      let left, remaining = parse_unary expr tokens in 
        parse_expr_aux left remaining

  and parse_unary expr tokens = 
    match tokens with
        | [] -> expr, [] 
        | x :: xs -> 
          match x.token_type with 
            | Minus -> 
                let right, rest = parse_power Ast.Nil xs 
                  in Ast.Negate right, rest
            | Keywords Not -> 
                let right, rest =  parse_power Ast.Nil xs 
                  in Ast.Not right, rest
            | _ -> 
              parse_power expr tokens
  
  and parse_power expr tokens = 
    let rec parse_expr_aux left remaining =
      match remaining with 
        | [] -> left, []
        | x :: xs -> 
          let right, rest = parse_power Ast.Nil xs in
            match x.token_type with 
              | Caret -> parse_expr_aux (Ast.Power (left, right)) rest
              | _ -> left, remaining
    in 
      let left, remaining = parse_primary expr tokens in 
        parse_expr_aux left remaining

  and parse_primary expr tokens = 
    match tokens with 
      | [] -> expr, []
      | x :: xs -> match x.token_type with 
        | Float x -> Ast.Float x, xs
        | Integer x ->  Ast.Int x, xs
        | String x -> Ast.String x, xs
        | Keywords Nil -> Ast.Nil, xs 
        | Punctuation LParen -> 
          let expr, rest = parse_expr expr xs in 
            (match rest with 
              | [] -> raise_invalid_token x "expected closing parenthesis"
              | x :: xs -> 
                if x.token_type != Punctuation RParen then Ast.Grouping expr, xs
                else raise_invalid_token x "expected closing parenthesis")
        | EOF -> expr, []
        | _ -> parse_expr expr xs
        (* | _ -> raise_invalid_token x "for primary expression" *)

let parse_expr tokens = let expr, _ = parse_expr Ast.Nil tokens in expr
