open Token;;

exception InvalidToken of string;;

let raise_invalid_token token extra = 
  let name, lexeme, line, col = token.name, token.lexeme, token.line, token.col in
  let details = String.concat "" [name; ", '"; lexeme; "' line "; string_of_int line; ", col "; string_of_int col] in
  let message = "Invalid token: (" ^ details ^ ") " ^ extra in 
  let exc = InvalidToken message in raise exc

module type ExprParser = sig 
  type t 
  val parse_expr : Ast.expr -> token list -> Ast.expr * token list
end

module type StmtParser = sig 
  type t 
  val parse_stmt : Ast.stmt -> token list -> Ast.stmt * token list
end

let rec parse_expr expr tokens = 
  let parse_primary expr tokens = 
    match tokens with 
      | [] -> expr, []
      | x :: xs -> match x.token_type with 
        | Float x -> Ast.Float x, xs
        | Integer x ->  Ast.Int x, xs
        | String x -> Ast.String x, xs
        | Keywords Nil -> Ast.Nil, xs 
        | Punctuation LParen -> 
          let expr, rest = parse_expr expr xs in 
          Ast.Grouping expr, rest 
        (* | Punctuation RParen -> expr, xs *)
        | EOF -> expr, []
        | _ -> parse_expr expr tokens
        (* | _ -> raise_invalid_token x "for primary expression" *)

  in 
    let rec parse_power ast tokens =
      let rec parse_expr_aux left remaining =
        match remaining with 
          | [] -> left, []
          | x :: xs -> 
            let right, rest = parse_power Ast.Nil xs in
              match x.token_type with 
                | Caret -> parse_expr_aux (Ast.Power (left, right)) rest
                | _ -> left, remaining
      in 
        let left, remaining = parse_primary ast tokens in 
          parse_expr_aux left remaining

  in 
    let parse_unary ast tokens =
      match tokens with
        | [] -> ast, [] 
        | x :: xs -> 
          match x.token_type with 
            | Minus -> 
                let right, rest = parse_power Ast.Nil xs 
                  in Ast.Negate right, rest
            | Keywords Not -> 
                let right, rest =  parse_power Ast.Nil xs 
                  in Ast.Not right, rest
            | _ -> 
              parse_power ast tokens

  in
    let parse_factor ast tokens =
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
        let left, remaining = parse_unary ast tokens in 
          parse_expr_aux left remaining
    
    in 
      let parse_term ast tokens =
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
          let left, remaining = parse_factor ast tokens in 
            parse_expr_aux left remaining
      
    in
      let parse_concat ast tokens =
        let rec parse_expr_aux left remaining =
          match remaining with 
            | [] -> left, []
            | x :: xs -> 
              let right, rest = parse_term Ast.Nil xs in
              match x.token_type with 
                | BinOp DotDot -> parse_expr_aux (Ast.Concat (left, right)) rest
                | _ -> left, remaining
        in 
          let left, remaining = parse_term ast tokens in 
            parse_expr_aux left remaining

    in 
      let parse_comparison ast tokens =
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
          let left, remaining = parse_concat ast tokens in 
            parse_expr_aux left remaining

    in 
      let parse_and_or ast tokens =
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
          let left, remaining = parse_comparison ast tokens in 
            parse_expr_aux left remaining
  in 
    parse_and_or expr tokens


let parse_expr tokens = let expr, _ = parse_expr Ast.Nil tokens in expr
