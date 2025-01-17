open Token;;
open Result;;

exception Invalid_token of string;;
exception Parse_error of string;;


(* Chain results easier*)
let ( let* ) r f = match r with 
  Ok v -> f v 
| Error e -> Error e

let raise_invalid_token token extra = 
  let name, lexeme, line, col = token.name, token.lexeme, token.line, token.col in
  let details = String.concat "" [name; ", '"; lexeme; "' line "; string_of_int line; ", col "; string_of_int col] in
  "Invalid token: (" ^ details ^ ") " ^ extra  

let peek tokens = match tokens with [] -> None | x :: _ -> Some x

let consume tokens = match tokens with [] -> None, [] | x :: xs -> Some x,  xs

let expect token_type_name token_type tokens =
  match tokens with 
    [] -> Error (Parse_error ("unexpected end of file, expected token type " ^ token_type_name))
    | x :: xs  -> match x with 
      | x when x.token_type = token_type -> Ok (Some x, xs)
      | x when get_tok_type x.token_type = token_type -> Ok (Some x, xs)
      | _ -> Error (Parse_error ("expected " ^ token_type_name ^ " but got " ^ stringify_token x))

let peek_res tokens msg = match tokens with 
  [] -> Error (Parse_error msg)
  | x :: _ -> Ok x

let accept token_type tokens  = 
  match tokens with [] -> None, tokens
  | x :: xs -> match x with 
    | x when get_tok_type x.token_type = token_type -> Some x, xs
    | x when token_type = x.token_type -> Some x, xs
    | _ -> None, tokens

let rec parse_expr expr tokens =
  parse_and_or expr tokens

and parse_and_or expr tokens =
  let rec parse_expr_aux left remaining =
    match remaining with 
      | [] -> Ok (left, [])
      | x :: xs -> 
        match x.token_type with 
          | Keywords And -> 
              let* right, rest = parse_comparison Ast.Nil xs in
                parse_expr_aux (Ast.And (left, right)) rest
          | Keywords Or -> 
              let* right, rest = parse_comparison Ast.Nil xs in
                parse_expr_aux (Ast.Or (left, right)) rest
          | _ -> Ok (left, remaining)
  in 
    let* left, remaining = parse_comparison expr tokens in 
      parse_expr_aux left remaining
  
and parse_comparison expr tokens = 
  let rec parse_expr_aux left remaining =
    match remaining with 
      | [] -> Ok (left, [])
      | x :: xs -> 
        match x.token_type with 
          | BinOp x -> 
            let* right, rest = parse_comparison Ast.Nil xs in
              (match x with 
                | Equal -> parse_expr_aux (Ast.Equal (left, right)) rest
                | Less -> parse_expr_aux (Ast.Less (left, right)) rest
                | Leq -> parse_expr_aux (Ast.Leq (left, right)) rest
                | Greater -> parse_expr_aux (Ast.Greater (left, right)) rest
                | Geq -> parse_expr_aux (Ast.Geq (left, right)) rest
                | NotEqual -> parse_expr_aux (Ast.Neq (left, right)) rest
                | _ -> Ok (left, remaining))
          | _ -> Ok (left, remaining)
    in 
      let* left, remaining = parse_concat expr tokens in 
        parse_expr_aux left remaining
    
and parse_concat expr tokens = 
  let rec parse_expr_aux left remaining =
    match remaining with 
      | [] -> Ok (left, [])
      | x :: xs -> 
        match x.token_type with 
          | BinOp DotDot ->         
            let* right, rest = parse_comparison Ast.Nil xs in
              parse_expr_aux (Ast.Concat (left, right)) rest
          | _ -> Ok (left, remaining)
  in 
    let* left, remaining = parse_term expr tokens in 
      parse_expr_aux left remaining
  
and parse_term expr tokens = 
  let rec parse_expr_aux left remaining =
    match remaining with 
      | [] -> Ok (left, [])
      | x :: xs -> 
        match x.token_type with 
          | Minus -> 
              let* right, rest = parse_comparison Ast.Nil xs in
                parse_expr_aux (Ast.Subtract (left, right)) rest
          | BinOp Plus -> 
              let* right, rest = parse_comparison Ast.Nil xs in
                parse_expr_aux (Ast.Add (left, right)) rest
          | _ -> Ok (left, remaining)
  in 
    let* left, remaining = parse_factor expr tokens in 
      parse_expr_aux left remaining

and parse_factor expr tokens = 
  let rec parse_expr_aux left remaining =
    match remaining with 
      | [] -> Ok (left, [])
      | x :: xs -> 
        match x.token_type with 
          | BinOp Star -> 
              let* right, rest = parse_comparison Ast.Nil xs in
                parse_expr_aux (Ast.Multiply (left, right)) rest
          | BinOp Slash -> 
              let* right, rest = parse_comparison Ast.Nil xs in
                parse_expr_aux (Ast.Divide (left, right)) rest
          | _ -> Ok (left, remaining)
  in 
    let* left, remaining = parse_unary expr tokens in 
      parse_expr_aux left remaining

and parse_unary expr tokens = 
  match tokens with
    | [] -> Ok (expr, [])
    | x :: xs -> 
      match x.token_type with 
        | Minus -> 
            let* right, rest = parse_power Ast.Nil xs 
              in Ok (Ast.Negate right, rest)
        | Keywords Not -> 
            let* right, rest =  parse_power Ast.Nil xs 
              in Ok (Ast.Not right, rest)
        | _ -> 
          parse_power expr tokens
  
and parse_power expr tokens = 
  let rec parse_power_aux left remaining =
    match remaining with 
      | [] -> Ok (left, [])
      | x :: xs -> 
        match x.token_type with 
          | Caret -> 
              let* right, rest = parse_unary Ast.Nil xs in
                parse_power_aux (Ast.Power (left, right)) rest
          | _ -> Ok (left, remaining)
  in 
    match parse_primary expr tokens with
      Ok (left, remaining) -> parse_power_aux left remaining
    | Error e -> Error e

and parse_primary expr tokens = 
  match tokens with 
    | [] -> Ok (expr, [])
    | x :: xs -> match x.token_type with 
      | Float x -> Ok (Ast.Float x, xs)
      | Integer i -> Ok (Ast.Int i, xs)
      | String x -> Ok (Ast.String x, xs)
      | Keywords True -> Ok (Ast.Boolean true, xs)
      | Keywords False -> Ok (Ast.Boolean false, xs)
      | Keywords Function -> parse_function xs
      | Ident i -> 
        let next = peek xs in (match next with 
          | Some t -> (match t.token_type with 
            | Punctuation LParen -> parse_fun_call_expr x (List.tl xs)
            |  _ -> Ok (Ast.Name i, xs))
          | None -> Ok (Ast.Name i, xs)) 
      | Keywords Nil -> Ok (Ast.Nil, xs)
      | Punctuation LParen -> 
        let* expr, rest = parse_expr expr xs in
          (match rest with 
            | [] -> Error (Parse_error (raise_invalid_token x "expected closing parenthesis"))
            | x :: xs -> match x.token_type with
              | Punctuation RParen -> 
                  let (g: Ast.expr) = Ast.Grouping expr in Ok (g, xs)
              | _ -> Error (Parse_error (raise_invalid_token x "expected closing parenthesis")))
      | EOF -> Ok (expr, [])
      | _ -> Ok (expr, tokens)

and parse_function tokens = 
  let* _, after_parens = expect "LPAREN" (Punctuation LParen) tokens in
  let* params, tokens_after_params = parse_params after_parens in 
  let* func_body_block, tokens_after_body = parse_block tokens_after_params in 
  let (func: Ast.expr) = Ast.Function {
      name = ""; 
      body = {
        params = List.rev params;
        block = func_body_block
      }
    } 
  in 
    Ok (func, tokens_after_body)

and parse_fun_call_expr func_name tokens  =
  let* args, tokens_after_args = parse_args tokens in 
  let (fc: Ast.expr) = Ast.FunctionCall {
    target = Ast.Name func_name.lexeme; 
    args = List.rev args;
  } 
  in 
    Ok(fc, tokens_after_args)

and parse_assignment name tokens is_local = 
  match tokens with 
    | [] -> Error (Parse_error ("unexpected end of file assigning identifier \"" ^ name ^ "\""))
    | x :: xs -> 
      let* expr, rest = parse_expr Ast.Dummy (x :: xs) in 
      let stmt = Ast.AssignStmt{
        name = name; 
        is_local = is_local; 
        right = expr
      } in 
        Ok (stmt, rest)

and parse_return_stmt tokens = 
  let* expr, rest = parse_expr Ast.Dummy tokens in 
    Ok (Ast.ReturnStmt (Some expr), rest)

and parse_params tokens: (string list * token list, exn) t  = 
  let rec parse_params_aux names tokens = 
    match tokens with
      [] -> Error (Parse_error ("unexpected end of file parsing parameter list"))
    | x :: xs -> match x.token_type with
        Punctuation RParen -> Ok (names, xs)
      | Punctuation Comma -> 
        let none_err_msg = "unexpected end of file in parameter list after comma " ^ stringify_token x in 
        let* next = peek_res xs none_err_msg in (match next.token_type with 
            Punctuation RParen -> Error (Parse_error ("expected argument after comma " ^ stringify_token x))
          | _ -> parse_params_aux names xs)
      | Ident _ -> 
        let* name, toks_after_ident = parse_ident (x :: xs) in
        let none_err_msg = "expected comma or closing parenthesis after identifier " ^ stringify_token x in
        let* next = peek_res toks_after_ident none_err_msg in (match next.token_type with 
          Punctuation RParen -> parse_params_aux (name :: names) toks_after_ident
        | _ -> 
          let* _, _ = expect "COMMA" (Punctuation Comma) toks_after_ident in
            parse_params_aux (name :: names) toks_after_ident)
      | _ -> Error (Parse_error ("expected an indentifier but got " ^ stringify_token x))
  in 
    parse_params_aux [] tokens

and parse_ident tokens: (string * token list, exn) t = 
  let* ident, tokens_after_ident = expect "IDENT" (TIdent) tokens in 
  match ident with 
    None -> Error (Parse_error "empty identifier")
  | Some i -> Ok (i.lexeme, tokens_after_ident)

and parse_args tokens = 
  let rec parse_args_aux params tokens = 
    match tokens with
      [] -> Error (Parse_error ("unexpected end of file parsing parameter list"))
    | x :: xs -> match x.token_type with
        Punctuation RParen -> Ok (params, xs)
      | Punctuation Comma -> 
        let none_err_msg = "unexpected end of file in parameter list after comma " ^ stringify_token x in 
        let* next = peek_res xs none_err_msg in (match next.token_type with 
            Punctuation RParen -> Error (Parse_error ("expected argument after comma " ^ stringify_token x))
          | _ -> parse_args_aux params xs)
      | _ -> 
        let* param, toks_after_arg = parse_expr Ast.Dummy (x :: xs) in
        let none_err_msg = "expected comma or closing parenthesis after identifier " ^ stringify_token x in
        let* next = peek_res toks_after_arg none_err_msg in (match next.token_type with 
          Punctuation RParen -> parse_args_aux (param :: params) toks_after_arg
        | _ -> 
          let* _, _ = expect "COMMA" (Punctuation Comma) toks_after_arg in
          parse_args_aux (param :: params) toks_after_arg)
  in 
    parse_args_aux [] tokens

and parse_block tokens = 
  let rec parse_block_aux block tokens = 
    match tokens with 
      | [] -> Error (Parse_error "unexpected end of file parsing block") 
      | x :: xs -> match x.token_type with 
          EOF | Keywords End -> Ok ({Ast.stmts = List.rev block.Ast.stmts}, xs)
        | Keywords Elseif | Keywords Else -> Ok ({Ast.stmts = List.rev block.Ast.stmts}, x :: xs)
        | _ -> 
          let* stmt, rest = parse_stmt tokens in
            let new_block = match stmt with 
              | _ -> {Ast.stmts = (stmt :: block.Ast.stmts)}
            in parse_block_aux new_block rest
  in
    parse_block_aux {Ast.stmts = []} tokens

and parse_stmt tokens =
  match tokens with 
    | [] -> Error (Parse_error "unexpected end of input")
    | x :: xs -> 
      (match x.token_type with 
      | Keywords While -> parse_while_loop xs
      | Keywords Function -> parse_function_def xs
      | Keywords If -> parse_if_stmt xs
      | Keywords Return -> parse_return_stmt xs
      | Keywords Break -> Ok (Ast.Break, xs)
      | Keywords Local -> 
        let* ident, after_ident = expect "IDENT" (TIdent) xs in 
          let name = match ident with Some v -> v.lexeme | None -> "" in
          let* _, after_equal = expect "EQUAL" (BinOp Assign) after_ident in
            parse_assignment name after_equal true
      | Ident name -> 
          let next = peek xs in (match next with 
          | Some t -> (match t.token_type with
            | BinOp Assign -> 
              let* _, after_equal = expect "EQUAL" (BinOp Assign) xs in
                parse_assignment name after_equal false
            | Punctuation LParen -> parse_fun_call_stmt name (List.tl xs)
            | _ -> Error (Parse_error ("unexpected token " ^ stringify_token t)))
          | None -> Error (Parse_error ("hanging indentifier " ^ stringify_token x))) 
      | _ -> Error (Parse_error ("unexpected token: " ^ stringify_token x))
    )

and parse_while_loop tokens = 
  let* condition, tokens_after_condition = parse_expr Ast.Dummy tokens in 
  let* _, after_do = expect "DO" (Keywords Do) tokens_after_condition in
  let* while_block, tokens_after_while_block = parse_block after_do in
      Ok (Ast.WhileLoop {
        while_condition = condition; 
        body = while_block
      }, tokens_after_while_block) 

and parse_if_stmt tokens = 
  let* condition, tokens_after_condition = parse_expr Ast.Dummy tokens in 
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

  Ok (Ast.IfStmt{
      then_condition = condition; 
      then_block = then_block; 
      elseif = else_if_block; 
      else_block = else_block
    }, tokens_after_else_if_block)
      
and parse_function_def tokens =
  let ident, tokens_after_ident = accept (TIdent) tokens in 
  let name = match ident with Some i -> i.lexeme | None -> "" in 
  let* _, after_parens = expect "LPAREN" (Punctuation LParen) tokens_after_ident in
  let* params, tokens_after_params = parse_params after_parens in 
  let* func_body_block, tokens_after_body = parse_block tokens_after_params in 
  let (func: Ast.expr) = Ast.Function {
    name = name; 
    body = {
      params = List.rev params;
      block = func_body_block
    }
  } in let stmt = Ast.AssignStmt{name = name; is_local = false; right = func} in
  Ok (stmt, tokens_after_body)

and parse_fun_call_stmt func_name tokens = 
  let* params, tokens_after_params = parse_args tokens in
  let func =  Ast.FunctionCall {
    target = Ast.Name func_name; 
    args = List.rev params;
  } in 
    Ok (func, tokens_after_params) 

and parse_program tokens = 
  parse_block tokens