open Token;;

exception InvalidToken of string;;

let raise_invalid_token token extra = 
  let (name, lexeme, line, col) = (token.name, token.lexeme, token.line, token.col) in
  let details = String.concat "" [name; ", '"; lexeme; "' line "; string_of_int line; ", col "; string_of_int col] in
  let message = "Invalid token: (" ^ details ^ ") " ^ extra in 
  let exc = InvalidToken message in raise exc

module type Parser = sig 
  type t 
  val parse_expr : Ast.expr -> token list -> Ast.expr * token list
end

module PrimaryParser = struct 
  type t 
  let parse_expr ast tokens =
    match tokens with 
      | [] -> (Ast.NilExp, [])
      | x :: xs -> match x.token_type with 
        | Float x -> (Ast.Float x, xs)
        | Integer x ->  (Ast.Int x, xs)
        | String x -> (Ast.String x, xs)
        | EOF -> (ast, [])
        | _ -> raise_invalid_token x "for primary expression"
end

module TermParser(P: Parser) = struct 
  type t 
  let parse_factor = P.parse_expr

  let  parse_expr ast tokens =
    let rec parse_expr_aux left remaining =
      match remaining with 
        | [] -> (ast, [])
        | x :: xs -> match x.token_type with 
        | Minus -> 
          let (right, rest) =  parse_factor Ast.NilExp xs in 
            parse_expr_aux (Ast.Subtract (left, right)) rest
        | BinOp x -> 
          (match x with 
          | Plus -> 
            let (right, rest) =  parse_factor Ast.NilExp xs in 
            parse_expr_aux (Ast.Add (left, right)) rest
          | _ -> parse_factor left remaining)
        | _ ->  
          parse_factor left remaining
  in 
      let (left, remaining) = parse_factor ast tokens in 
      parse_expr_aux left remaining
end

module FactorParser(P: Parser) = struct 
  type t 
  let parse_primary = P.parse_expr

  let parse_expr ast tokens =
    let (left, remaining) = parse_primary ast tokens in
    match remaining with 
      | [] -> (ast, [])
      | x :: xs -> match x.token_type with 
      | BinOp x -> 
        (match x with 
        | Star -> 
          let (right, rest) =  parse_primary Ast.NilExp xs in 
           (Ast.Multiply (left, right), rest)
        | Slash -> 
          let (right, rest) =  parse_primary Ast.NilExp xs in 
           (Ast.Divide (left, right), rest)
        | _ -> (left, remaining))
      | _ -> 
        (left, remaining)
end

module TFactorParser = FactorParser(PrimaryParser)
module TTermParser = TermParser(TFactorParser)
module ExpressionParser = TTermParser