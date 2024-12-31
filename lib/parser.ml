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

let peek tokens = match tokens with [] -> None | x :: _ -> Some x

let consume tokens = match tokens with [] -> None, [] | x :: xs -> Some x,  xs

let expect token_type_name token_type tokens =
  match tokens with 
    [] -> Error (ParseError ("unexpected end of file, expected token type " ^ token_type_name))
    | x :: xs  -> match x with 
      | x when x.token_type = token_type -> Ok (Some x, xs)
      | x when get_tok_type x.token_type = token_type -> Ok (Some x, xs)
      | _ -> Error (ParseError ("expected " ^ token_type_name ^ " but got " ^ stringify_token x))

let peek_res tokens msg = match tokens with 
  [] -> Error (ParseError msg)
  | x :: _ -> Ok x

let accept token_type tokens  = 
  match tokens with [] -> None, tokens
  | x :: xs -> match x with 
    | x when token_type = x.token_type -> Some x, xs
    | _ -> None, tokens

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
            | Punctuation LParen -> parse_fun_call_expr x xs
            | _ -> (Ast.Name x, xs))
          | None -> (Ast.Name x, xs)) 
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

and parse_fun_call_expr func_name tokens = 
  let params_result = parse_params tokens in match params_result with 
    Ok (params, tokens_after_params) -> 
      (Ast.FunctionCall {
        target = Ast.Name func_name; 
        args = List.rev params;
      }, tokens_after_params) 
  | Error e -> raise e

and parse_assignment name tokens = 
  match tokens with 
    | [] | [_] -> Error (ParseError ("unexpected end of file assigning identifier \"" ^ name ^ "\""))
    | _ -> let expr, rest = parse_expr Ast.Nil tokens in 
    Ok (Ast.AssignStmt{ident = name; right = expr}, rest)

and parse_return_stmt tokens = 
  let expr, rest = parse_expr Ast.Nil tokens in 
    Ok (Ast.LastStmt (ReturnStmt (Some expr)), rest)

and parse_params tokens = 
  let next, rest = consume tokens in match next with 
    None -> Error (ParseError ("unexpected end of file parsing parameter list"))
  | Some token -> match token.token_type with 
    Punctuation LParen -> 
      let rec parse_params_aux params tokens = 
        match tokens with
          [] -> Error (ParseError ("unexpected end of file parsing parameter list after " ^ stringify_token token))
        | x :: xs -> match x.token_type with
          Punctuation RParen -> Ok (params, xs)
          | Ident _ -> 
            let param, toks_after_ident = parse_expr Ast.Nil (x :: xs) in
            let err_msg = "expected comma or closing parenthesis after identifier " ^ stringify_token x in
            let* next = peek_res toks_after_ident err_msg in (match next.token_type with 
              Punctuation RParen -> parse_params_aux (param :: params) toks_after_ident
            | _ -> let* _, _ = expect "COMMA" (Punctuation Comma) toks_after_ident in
                parse_params_aux (param :: params) toks_after_ident)
          | Punctuation Comma -> 
            let err_msg = "expected end of file in parameter list after comma " ^ stringify_token x in 
            let* next = peek_res xs err_msg in (match next.token_type with 
              Ident _ -> parse_params_aux params xs
            | _ -> Error (ParseError ("expected identifier param after comma " ^ stringify_token x)))
          | _ -> 
            Error (ParseError ("expected identifier after " ^ stringify_token token ^ " but got " ^ stringify_token x))
      in 
        parse_params_aux [] rest
    | _ -> Error (ParseError ("expected openining parenthesis, got " ^ stringify_token token))

and parse_block tokens = 
  let rec parse_block_aux block tokens = 
    match tokens with 
      | [] -> Error (ParseError "unexpected end of file parsing block") 
      | x :: xs -> match x.token_type with 
          EOF | Keywords End -> Ok(block, xs)
           | Keywords Elseif | Keywords Else -> Ok(block, x :: xs)
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
            | Punctuation LParen -> parse_fun_call_stmt name xs
            | _ -> Error (ParseError ("unexpected token " ^ stringify_token t)))
          | None -> Error (ParseError ("hanging indentifier " ^ stringify_token x))) 
      | _ -> Error (ParseError ("unexpected token: " ^ stringify_token x))
    )

and parse_while_loop tokens = 
  let condition, tokens_after_condition = parse_expr Ast.Nil tokens in 
  let* _, after_do = expect "DO" (Keywords Do) tokens_after_condition in
  let* while_block, tokens_after_while_block = parse_block after_do in
      Ok (Ast.WhileLoop {
        condition = condition; 
        body = while_block
      }, tokens_after_while_block) 

and parse_if_stmt tokens = 
  let condition, tokens_after_condition = parse_expr Ast.Nil tokens in 
  let* _, after_then = expect "THEN" (Keywords Then) tokens_after_condition in 
  let* then_block, tokens_after_then_block = parse_block after_then in

  let maybe_else_token = accept (Keywords Else) tokens_after_then_block in 
  let* else_block, tokens_after_else_block = match maybe_else_token with 
      None, _ -> Ok (Ast.empty_block, tokens_after_then_block) 
    | Some _, after_else -> parse_block after_else 
  in 
  let else_block = Ast.make_optional else_block in

  let maybe_elseif_token = accept (Keywords Elseif) tokens_after_else_block in 
  let* else_if_block, tokens_after_else_if_block = match maybe_elseif_token with 
      None, _ -> Ok (Ast.empty_if, tokens_after_else_block) 
    | Some _, after_elseif -> parse_if_stmt after_elseif 
  in 
  let else_if_block = Ast.make_optional_if (Ast.stmt_to_if else_if_block) in 

  Ok(Ast.IfStmt{condition = condition; 
        then_block = then_block; 
        elseif = else_if_block; 
        else_block = else_block}, 
        tokens_after_else_if_block)
      
and parse_function_def tokens =
  let* ident, tokens_after_ident = expect "IDENT" (TIdent) tokens in 
  let name = match ident with Some i -> i.lexeme | None -> "" in 
  let* params, tokens_after_params = parse_params tokens_after_ident in 
  let* func_body_block, tokens_after_body = parse_block tokens_after_params in 
  Ok (Ast.Function {
    name = name; 
    body = {
      params = params;
      block = func_body_block
    }
  }, tokens_after_body)

and parse_fun_call_stmt func_name tokens = 
  let* params, tokens_after_params = parse_params tokens in 
    Ok (Ast.FunctionCall {
      target = Ast.Name func_name; 
      args = List.rev params;
    }, tokens_after_params) 

and parse_program tokens = 
  parse_block tokens