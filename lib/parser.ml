open Token;;


(* module type Parser = sig 
  type t = {
    tokens: token list; 
    current: token; 
    prev: token;
    remaining: token list;
    ast: Ast.expr;
  }

  val make : token list -> t
  val parse_exp : t -> t
  (* val parse_bin_exp : t -> Ast.expr *)
end 

module ExpressionParser : Parser = struct 
  type t = {
    tokens: token list; 
    current: token; 
    prev: token;
    remaining: token list;
    ast: Ast.expr;
  }
  exception InvalidToken of string;;

  let make tokens = {tokens = tokens; remaining = tokens; current = (List.nth tokens 0); 
  prev = Token.make "" EOF "" 0 0; ast = Ast.NilExp}
  let raise_invalid_token token extra = 
    let (name, lexeme, line, col) = (token.name, token.lexeme, token.line, token.col) in
    let details = String.concat "" [name; ", '"; lexeme; "' line "; string_of_int line; ", col "; string_of_int col] in
    let message = "Invalid token: (" ^ details ^ ") " ^ extra in 
    let exc = InvalidToken message in raise exc


  (* let current (t: t) = t.current
  let prev (t: t) = t.prev

  let peek (t: t) = List.nth t.remaining 0*)

  let advance (t: t): t = match t.remaining with 
    | [] -> {t with current = Token.make "" EOF "" 0 0; remaining = []}
    | x :: xs -> 
      let prev = t.current in {t with prev = prev; current = x; remaining = xs}

  let parse_exp (t) : t = 
    (* if t.current.token_type = EOF then t.ast else *)
    let rec parse_exp_aux t = 
      let expr = match t.remaining with 
        | [] -> Ast.NilExp
        | x :: _ ->
          match x.token_type with 
          | Float x -> {t with ast = Ast.Float x}
          | Integer x -> {t with ast = Ast.Int x}
          | BinOp Plus ->  
            let r = parse_exp_aux (advance t) in 
            {t with ast = Ast.Add (t.ast, r.ast)}
          | EOF -> t.ast
          | _ -> raise_invalid_token x "for expression"
      in 
        (* let ast_str = Ast.stringify_expr new_t.ast in 
        let _ = print_string ast_str in  *)
        parse_exp_aux (advance {t with ast = expr})
  in 
    parse_exp_aux t

  (* let parse_bin_exp (t) : Ast.expr = 
    (* let left = prev t in  *)
    let op = peek t in 
    (* let right = peek t in  *)
    match op.token_type with 
      | BinOp Plus -> Ast.Add(parse_exp t, parse_exp t)
      | _ -> raise_invalid_token op "for binary expression" *)
end  *)

exception InvalidToken of string;;

let raise_invalid_token token extra = 
  let (name, lexeme, line, col) = (token.name, token.lexeme, token.line, token.col) in
  let details = String.concat "" [name; ", '"; lexeme; "' line "; string_of_int line; ", col "; string_of_int col] in
  let message = "Invalid token: (" ^ details ^ ") " ^ extra in 
  let exc = InvalidToken message in raise exc

let parse_exp tokens = 
  let rec parse_exp_aux ast tokens = 
    match tokens with 
      | [] -> (ast, [])
      | x :: xs -> match x.token_type with 
        | Float x -> parse_exp_aux (Ast.Float x) xs
        | Integer x -> parse_exp_aux (Ast.Int x) xs
        | BinOp x -> 
            let (right, rest) = parse_exp_aux Ast.NilExp xs in
            (match x with 
            | Plus -> parse_exp_aux (Ast.Add (ast, right)) rest
            | Star -> parse_exp_aux (Ast.Multiply (ast, right)) rest
            | Slash -> parse_exp_aux (Ast.Divide (ast, right)) rest
            | _ -> (Ast.NilExp, []))
        | EOF -> (ast, [])
        | _ -> raise_invalid_token x "for expression"
in 
  parse_exp_aux Ast.NilExp tokens