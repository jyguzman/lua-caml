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
      | [] -> (ast, [])
      | x :: xs -> match x.token_type with 
        | Float x -> (Ast.Float x, xs)
        | Integer x ->  (Ast.Int x, xs)
        | String x -> (Ast.String x, xs)
        | EOF -> (ast, [])
        | _ -> raise_invalid_token x "for primary expression"
end

module ComparisonParser(P: Parser) = struct 
  type t 
  let parse_term = P.parse_expr

  let  parse_expr ast tokens =
    let rec parse_expr_aux left remaining =
      match remaining with 
        | [] -> (left, [])
        | x :: xs -> 
          let (right, rest) = parse_term Ast.NilExp xs in
          match x.token_type with 
            | BinOp Equal -> parse_expr_aux (Ast.Equal (left, right)) rest
            | BinOp Less -> parse_expr_aux (Ast.Less (left, right)) rest
            | BinOp Leq -> parse_expr_aux (Ast.Leq (left, right)) rest
            | BinOp Greater -> parse_expr_aux (Ast.Greater (left, right)) rest
            | BinOp Geq -> parse_expr_aux (Ast.Geq (left, right)) rest
            | BinOp NotEqual -> parse_expr_aux (Ast.Neq (left, right)) rest
            | _ -> left, remaining
    in 
      let (left, remaining) = parse_term ast tokens in 
        parse_expr_aux left remaining
end

module TermParser(P: Parser) = struct 
  type t 
  let parse_factor = P.parse_expr

  let  parse_expr ast tokens =
    let rec parse_expr_aux left remaining =
      match remaining with 
        | [] -> (left, [])
        | x :: xs -> 
          let (right, rest) = parse_factor Ast.NilExp xs in
          match x.token_type with 
            | Minus -> parse_expr_aux (Ast.Subtract (left, right)) rest
            | BinOp Plus -> parse_expr_aux (Ast.Add (left, right)) rest
            | _ -> left, remaining
    in 
      let (left, remaining) = parse_factor ast tokens in 
        parse_expr_aux left remaining
end

module FactorParser(P: Parser) = struct 
  type t 
  let parse_unary = P.parse_expr

  let parse_expr ast tokens =
    let rec parse_expr_aux left remaining =
      match remaining with 
        | [] -> (left, [])
        | x :: xs -> 
          let (right, rest) =  parse_unary Ast.NilExp xs in
          match x.token_type with 
            | BinOp Star -> parse_expr_aux (Ast.Multiply (left, right)) rest
            | BinOp Slash -> parse_expr_aux (Ast.Divide (left, right)) rest
            | _ -> left, remaining
    in 
      let (left, remaining) = parse_unary ast tokens in 
        parse_expr_aux left remaining

end

module UnaryParser(P: Parser) = struct 
  type t

  let parse_primary = P.parse_expr
  let rec parse_expr ast tokens =
    match tokens with
      | [] -> (ast, []) 
      | x :: xs -> 
        match x.token_type with 
          | Minus -> let (right, rest) =  parse_expr Ast.NilExp xs in 
            (Ast.Negate right, rest)
          | Keywords Not -> let (right, rest) =  parse_expr Ast.NilExp xs in 
            (Ast.Not right, rest)
          | _ ->
            parse_primary ast tokens
end

module TUnaryParser = UnaryParser(PrimaryParser)
module TFactorParser = FactorParser(TUnaryParser)
module TTermParser = TermParser(TFactorParser)
module TComparisonParser = ComparisonParser(TTermParser)
module ExpressionParser = TComparisonParser

let parse_expr tokens = let (expr, _) = ExpressionParser.parse_expr Ast.NilExp tokens in expr