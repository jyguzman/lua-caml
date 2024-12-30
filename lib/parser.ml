open Token;;
open Result;;

exception InvalidToken of string;;
exception ParseError of string;;


(* Chain results easier*)
let ( let* ) r f = match r with 
  Ok v -> f v 
| Error e -> Error e

let raise_invalid_token token extra = 
  let name, lexeme, line, col = token.name, token.lexeme, token.line, token.col in
  let details = String.concat "" [name; ", '"; lexeme; "' line "; string_of_int line; ", col "; string_of_int col] in
  let message = "Invalid token: (" ^ details ^ ") " ^ extra in 
  let exc = InvalidToken message in raise exc

let look_ahead tokens n = List.nth_opt tokens n

let peek tokens = match tokens with [] -> None | x :: _ -> Some x

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
        let right, rest = parse_unary Ast.Nil xs in
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
      | Ident x -> 
        let next = peek xs in (match next with 
          | Some t -> (match t.token_type with 
            | Punctuation LParen -> (Ast.FunctionCall{target = Ast.Var x; args = Ast.String("arg")}, xs)
            | _ -> (Ast.Name x, xs))
          | _ -> (Ast.Name x, xs)) 
      | Keywords Nil -> Ast.Nil, xs 
      | Punctuation LParen -> 
        let expr, rest = parse_expr expr xs in 
          (match rest with 
            | [] -> raise_invalid_token x "expected closing parenthesis"
            | x :: xs -> match x.token_type with
              | Punctuation RParen -> Ast.Grouping expr, xs
              | _ -> raise_invalid_token x "expected closing parenthesis")
      | EOF -> expr, []
      | _ -> expr, tokens

let expect token expected_type = token.token_type == expected_type

let parse_expr__ tokens = let expr, _ = parse_expr Ast.Nil tokens in expr

let make_block stmts last_stmt = {Ast.stmts = stmts; Ast.last_stmt = last_stmt}

let consume tokens = match tokens with [] -> None, [] | x :: xs -> Some x,  xs

let parse_assignment name tokens = 
  match tokens with 
    | [] | [_] -> Error (ParseError ("unexpected end of file assigning identifier \"" ^ name ^ "\""))
    | _ -> let expr, rest = parse_expr Ast.Nil tokens in 
    Ok (Ast.AssignStmt{ident = name; right = expr}, rest)

let parse_return_stmt tokens = 
  let expr, rest = parse_expr Ast.Nil tokens in 
    Ok (Ast.LastStmt (ReturnStmt (Some expr)), rest)

let parse_params tokens = 
  let next, rest = consume tokens in match next with 
    None -> Error (ParseError ("unexpected end of file parsing parameter list"))
  | Some token -> match token.token_type with 
    Punctuation LParen -> 
      let rec parse_params_aux params tokens = match tokens with
          [] -> Error (ParseError ("unexpected end of file parsing parameter list after " ^ stringify_token token))
        | x :: xs -> match x.token_type with
          Punctuation RParen -> Ok (params, xs)
          | Ident _ -> 
              let param, rest = parse_expr Ast.Nil (x :: xs) in 
                parse_params_aux (param :: params) rest
          | Punctuation Comma -> 
            let next = peek xs in (match next with
                None -> Error (ParseError ("expected end of file in parameter list after comma " ^ stringify_token x))
              | Some t -> match t.token_type with 
                  Ident _ -> parse_params_aux params xs
                  | _ -> Error (ParseError ("expected identifier param after comma " ^ stringify_token x)))
          | _ -> Error (ParseError ("expected identifier after " ^ stringify_token token))
      in 
        parse_params_aux [] rest
    | _ -> Error (ParseError ("expected openining parenthesis, got " ^ stringify_token token))

let rec parse_block tokens = 
  let rec parse_block_aux block tokens = 
    match tokens with 
      | [] -> Error (ParseError "unexpected end of file parsing block") 
      | x :: xs -> match x.token_type with 
          EOF -> Ok(block, xs)
          | Keywords End | Keywords Elseif | Keywords Else -> Ok(block, x :: xs)
        | _ -> 
          let* stmt, rest = parse_stmt tokens in 
            let new_block = match stmt with 
                Ast.LastStmt stmt -> {block with Ast.last_stmt = Some stmt}
              | _ -> {block with Ast.stmts = (stmt :: block.Ast.stmts)}
            in parse_block_aux new_block rest
  in
    parse_block_aux {Ast.stmts = []; last_stmt = None} tokens

and parse_stmt tokens = 
  match tokens with 
    | [] -> Error (ParseError "unexpected end of input")
    | x :: xs -> 
      (match x.token_type with 
      | Keywords While -> parse_while_loop xs
      | Keywords Function -> parse_function_def xs
      | Keywords If -> parse_if_stmt xs
      | Keywords Return -> parse_return_stmt xs
      | Keywords Break -> Ok (Ast.LastStmt Ast.Break, xs)
      | Ident name -> 
          let next = peek xs in (match next with 
          | Some t -> (match t.token_type with
            | BinOp Assign -> parse_assignment name (List.tl xs)
            (* | Punctuation RParen -> parse_fun_call tokens *)
            | _ -> Error (ParseError ("unexpected token " ^ stringify_token t)))
          | None -> Error (ParseError ("hanging indentifier " ^ stringify_token x))) 
      | _ -> Error (ParseError ("unexpected token: " ^ stringify_token x))
    )

and parse_while_loop tokens = 
  let condition, rest = parse_expr Ast.Nil tokens in match rest with 
    [] -> Error (ParseError ("unexpected end of file after while condition"))
    | x :: xs -> match x.token_type with 
      Keywords Do -> 
        let* while_block, tokens_after_block = parse_block xs in 
        (match tokens_after_block with 
          [] -> Error (ParseError ("unexpected end of file after while loop after token " ^ stringify_token x))
          | x :: xs -> (match x.token_type with
            Keywords End -> Ok (Ast.WhileLoop {
              condition = condition; 
              body = while_block
            }, xs)
            | _ -> Error (ParseError ("expected 'end' after while loop, got " ^ stringify_token x))))
          
      | _ -> Error (ParseError ("expected 'do' after while condition, got " ^ stringify_token x))

and parse_if_stmt tokens = 
  let condition, rest = parse_expr Ast.Nil tokens in match rest with 
    [] -> Error (ParseError ("unexpected end of file after if condition"))
    | x :: xs -> (match x.token_type with
      Keywords Then -> 
        let* then_block, tokens_after_then_block = parse_block xs in 
        (match tokens_after_then_block with 
          [] -> Error (ParseError ("unexpected end of file after then block " ^ stringify_token x))
          | x :: xs -> (match x.token_type with 
            | Keywords Else -> 
              let* else_block, tokens_after_else = parse_block xs in 
                Ok(Ast.IfStmt{condition = condition; then_block = then_block; 
                elseif = None; else_block = Some else_block}, tokens_after_else)
                
            | Keywords End -> 
              Ok(Ast.IfStmt{condition = condition; then_block = then_block; 
                elseif = None; else_block = None}, xs)

            | _ -> Error (ParseError ("unexpected token " ^ stringify_token x))))
      | _ -> Error (ParseError ("expected then after if condition, got " ^ stringify_token x)))
      
and parse_function_def tokens =
  match tokens with 
    | [] -> Error (ParseError "unexpected end of file while defining function")
    | x :: xs -> match x.token_type with 
      Ident name ->  
        let* params, tokens_after_params = parse_params xs in 
        let* func_body_block, tokens_after_body = parse_block tokens_after_params in
          Ok(Ast.Function {
                name = name; 
                body = {
                  params = params;
                  block = func_body_block
                }
              }, List.tl tokens_after_body)
      | _ -> Error (ParseError ("expected identifier for function, got " ^ stringify_token x))