open Token;;
open Result;;

exception InvalidToken of string;;
exception ParseError of string;;

let raise_invalid_token token extra = 
  let name, lexeme, line, col = token.name, token.lexeme, token.line, token.col in
  let details = String.concat "" [name; ", '"; lexeme; "' line "; string_of_int line; ", col "; string_of_int col] in
  let message = "Invalid token: (" ^ details ^ ") " ^ extra in 
  let exc = InvalidToken message in raise exc

let look_ahead tokens n = List.nth_opt tokens n

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
        let peek = look_ahead xs 0 in (match peek with 
          | Some t -> (match t.token_type with 
            | Punctuation RParen -> (Ast.FunctionCall{target = Ast.Var x; args = Ast.String("arg")}, xs)
            | _ -> raise_invalid_token t "invalid token after identifier")
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

let expect token expected_type = token.token_type == expected_type

let parse_expr__ tokens = let expr, _ = parse_expr Ast.Nil tokens in expr

let make_block stmts last_stmt = {Ast.stmts = stmts; Ast.last_stmt = last_stmt}

let parse_fun_call _tokens = Ok (Ast.FunctionCall{target = Ast.Var("func call"); args = Ast.String("arg")})

let parse_function_def _tokens = Ok (
    Ast.FunctionDeclaration{name = "func_name"; body = {params = [];  block = make_block [] None}}
  )

let parse_while_loop _tokens = Ok(Ast.WhileLoop{condition = Ast.Nil; body = make_block [] None})

let parse_if_stmt _tokens = Ok(
  Ast.IfStmt{
    condition = Ast.Nil; 
    block = make_block [] None; 
    else_if = None; 
    else_block = None
})

let parse_assignment name tokens = 
  match tokens with 
    | [] | [_] -> Error (ParseError ("unexpected end of file assigning identifier \"" ^ name ^ "\""))
    | _ -> let expr, rest = parse_expr Ast.Nil tokens in 
    Ok (Ast.AssignStmt{ident = name; right = expr}, rest)

let parse_return_stmt tokens = 
  let expr, rest = parse_expr Ast.Nil tokens in 
    Ok (Ast.LastStmt (ReturnStmt expr), rest)

let parse_param_list tokens = 
  match tokens with 
    | [] -> Error (ParseError ("unexpected end of file parsing parameter list"))
    | x :: _ -> 
      match x.token_type with 
        | Punctuation LParen -> 
            let rec parse_params tokens params = 
              match tokens with 
                | [] -> Error (ParseError ("unexpected end of file parsing parameter list after " ^ stringify_token x)) 
                | x :: xs -> (match x.token_type with 
                  | Punctuation RParen -> Ok (xs, params)
                  | _ -> 
                    let (param, rest) = parse_expr Ast.Nil xs in 
                      parse_params rest (param :: params))
            in 
              parse_params tokens []
        | _ -> Error (ParseError ("expected openining parenthesis, got " ^ stringify_token x))

let rec parse_stmt tokens = 
  match tokens with 
    | [] -> Error (ParseError "unexpected end of input")
    | x :: xs -> 
      (match x.token_type with 
      (* | Keywords Function -> parse_function_def xs
      | Keywords While -> parse_while_loop xs
      | Keywords If -> parse_if_stmt xs *)
      | Keywords Return -> parse_return_stmt xs
      | Ident name -> 
          let peek = look_ahead xs 0 in (match peek with 
          | Some t -> (match t.token_type with
            | BinOp Assign -> parse_assignment name (List.tl xs)
            (* | Punctuation RParen -> parse_fun_call tokens *)
            | _ -> Error (ParseError ("unexpected token " ^ stringify_token t)))
          | None -> Error (ParseError ("hanging indentifier " ^ stringify_token x))) 
      | _ -> Error (ParseError ("unexpected token: " ^ stringify_token x))
    )

and parse_block tokens = 
  let rec parse_block_aux tokens stmts = 
    match tokens with 
      | [] -> Error (ParseError "unexpected end of file parsing block") 
      | x :: xs -> 
        (match x.token_type with 
          | EOF | Keywords End -> Ok(List.rev stmts, xs)
          | _ -> 
            let parse_stmt_result = parse_stmt tokens in 
              match parse_stmt_result with 
                | Ok (stmt, rest) -> parse_block_aux rest (stmt :: stmts) 
                | Error e -> Error e)
  in
    let block_result = parse_block_aux tokens [] in 
    match block_result with 
      | Ok (stmts, rest) -> Ok ({Ast.stmts = stmts; last_stmt = None}, rest)
      | Error e -> Error e

and parse_function_def tokens =
  match tokens with 
    | [] | [_] -> Error (ParseError "unexpected end of file while defining function")
    | x :: xs -> 
      match x.token_type with 
        | Ident x -> 
          let func_name = x in (match xs with 
            | []) 
        | _ -> Error (ParseError ("expected identifier for function, got " ^ stringify_token x))


