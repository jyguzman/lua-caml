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

module PrimaryParser = struct 
  type t 
  let parse_expr ast tokens =
    match tokens with 
      | [] -> ast, []
      | x :: xs -> match x.token_type with 
        | Float x -> Ast.Float x, xs
        | Integer x ->  Ast.Int x, xs
        | String x -> Ast.String x, xs
        | Keywords Nil -> Ast.Nil, xs
        | EOF -> ast, []
        | _ -> raise_invalid_token x "for primary expression"
end

module AndOrParser(P: ExprParser) = struct 
  type t 
  let parse_comparison = P.parse_expr

  let  parse_expr ast tokens =
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
end

module ComparisonParser(P: ExprParser) = struct 
  type t 
  let parse_concat = P.parse_expr

  let  parse_expr ast tokens =
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
end

module ConcatParser(P: ExprParser) = struct 
  type t 
  let parse_term = P.parse_expr

  let  parse_expr ast tokens =
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
end 

module TermParser(P: ExprParser) = struct 
  type t 
  let parse_factor = P.parse_expr

  let  parse_expr ast tokens =
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
end

module FactorParser(P: ExprParser) = struct 
  type t 
  let parse_unary = P.parse_expr

  let parse_expr ast tokens =
    let rec parse_expr_aux left remaining =
      match remaining with 
        | [] -> left, []
        | x :: xs -> 
          let right, rest =  parse_unary Ast.Nil xs in
          match x.token_type with 
            | BinOp Star -> parse_expr_aux (Ast.Multiply (left, right)) rest
            | BinOp Slash -> parse_expr_aux (Ast.Divide (left, right)) rest
            | _ -> left, remaining
    in 
      let left, remaining = parse_unary ast tokens in 
        parse_expr_aux left remaining

end

module UnaryParser(P: ExprParser) = struct 
  type t

  let parse_power = P.parse_expr
  let parse_expr ast tokens =
    match tokens with
      | [] -> ast, [] 
      | x :: xs -> 
        match x.token_type with 
          | Minus -> 
              let right, rest = parse_power Ast.Nil xs in Ast.Negate right, rest
          | Keywords Not -> 
              let right, rest =  parse_power Ast.Nil xs in Ast.Not right, rest
          | _ -> 
            parse_power ast tokens
end

module PowerParser(P: ExprParser)= struct
  type t
  let parse_primary = P.parse_expr

  let rec parse_expr ast tokens =
    let rec parse_expr_aux left remaining =
      match remaining with 
        | [] -> left, []
        | x :: xs -> 
          let right, rest = parse_expr Ast.Nil xs in
            match x.token_type with 
              | Caret -> parse_expr_aux (Ast.Power (left, right)) rest
              | _ -> left, remaining
    in 
      let left, remaining = parse_primary ast tokens in 
        parse_expr_aux left remaining
end 
(* module PowerParser(P: ExprParser)= struct
  type t
  let parse_primary = P.parse_expr

  let rec parse_expr ast tokens =
    let rec parse_expr_aux left remaining =
      match remaining with 
        | [] -> left, []
        | x :: xs -> 
          let right, rest = parse_expr Ast.Nil xs in
            match x.token_type with 
              | Caret -> parse_expr_aux (Ast.Power (left, right)) rest 
              | Minus -> Ast.Negate right, rest
              | Keywords Not -> Ast.Not right, rest
              | _ -> left, remaining
    in 
      match tokens with 
        | [] -> ast, []
        | _ :: [] -> parse_primary ast tokens
        | x :: y :: _ -> match x.token_type, y.token_type with 
          | Minus, _ | Keywords Not, _ -> parse_expr_aux ast tokens
          | _, Minus -> parse_primary ast tokens
          | _ -> let left, remaining = parse_primary ast tokens in 
              parse_expr_aux left remaining
end  *)

module TPowerParser = PowerParser(PrimaryParser)
module TUnaryParser = UnaryParser(TPowerParser)
module TFactorParser = FactorParser(TUnaryParser)
module TTermParser = TermParser(TFactorParser)
module TConcatParser = ConcatParser(TTermParser)
module TComparisonParser = ComparisonParser(TConcatParser)
module TAndOrParser = AndOrParser(TComparisonParser)
module ExpressionParser = TAndOrParser

let parse_expr tokens = let expr, _ = ExpressionParser.parse_expr Ast.Nil tokens in expr
