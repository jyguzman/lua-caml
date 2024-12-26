type unary_op_token_type = Caret | UnaryMinus

type bin_op_token_type = 
  | Less 
  | Leq
  | Greater
  | Geq
  | Equal
  | NotEqual
  | Assign
  | Plus
  | Slash 
  | Star
  | Dot
  | DotDot

type keywords = 
  | Function 
  | Do 
  | End 
  | Not | Or | And
  | For | While

type punctuation = 
  | Comma 
  | LBrace | RBrace 
  | LBracket | RBracket 
  | LParen | RParen 

type token_type = 
  | Integer of int
  | Float of float
  | String of string
  | Ident of string
  | Caret
  | Minus
  | Dummy
  | BinOp of bin_op_token_type
  | Keywords of keywords

type token = {
  token_type: token_type;
  lexeme: string;
  line: int;
  col: int
}

let stringify_token token = 
  let col = token.col in 
  let line = token.line in match token.token_type with 
    | Dummy -> ""
    | Integer x -> String.concat "" ["Integer("; string_of_int x; " "; string_of_int line; ":"; string_of_int col; ")"]
    | Float x -> String.concat "" ["Float("; string_of_float x; " "; string_of_int line; ":"; string_of_int col; ")"]
    | String x -> String.concat "" ["String("; x; " "; string_of_int line; ":"; string_of_int col; ")"]
    | Ident x -> String.concat "" ["Ident("; x; " "; string_of_int line; ":"; string_of_int col; ")"]
    | Caret -> String.concat "" ["Caret(^"; " "; string_of_int line; ":"; string_of_int col; ")"] 
    | Minus -> String.concat "" ["Minus(-"; " "; string_of_int line; ":"; string_of_int col; ")"]
    | BinOp x -> (match x with 
      | Less -> String.concat "" ["Less(<"; " "; string_of_int line; ":"; string_of_int col; ")"]
      | Leq -> String.concat "" ["Leq(<="; " "; string_of_int line; ":"; string_of_int col; ")"]
      | Greater -> String.concat "" ["Greater(>"; " "; string_of_int line; ":"; string_of_int col; ")"]
      | Geq -> String.concat "" ["Geq(>="; " "; string_of_int line; ":"; string_of_int col; ")"]
      | Equal -> String.concat "" ["Equal(=="; " "; string_of_int line; ":"; string_of_int col; ")"]
      | NotEqual -> String.concat "" ["NotEqual(~="; " "; string_of_int line; ":"; string_of_int col; ")"]
      | Assign -> String.concat "" ["Assign(="; " "; string_of_int line; ":"; string_of_int col; ")"]
      | Plus -> String.concat "" ["Plus(+"; " "; string_of_int line; ":"; string_of_int col; ")"]
      | Slash -> String.concat "" ["Slash(/"; " "; string_of_int line; ":"; string_of_int col; ")"]
      | Star -> String.concat "" ["Star(*"; " "; string_of_int line; ":"; string_of_int col; ")"]
      | Dot -> String.concat "" ["Dot(."; " "; string_of_int line; ":"; string_of_int col; ")"]
      | DotDot -> String.concat "" ["DotDot(.."; " "; string_of_int line; ":"; string_of_int col; ")"])
    | Keywords x -> match x with 
      | Function -> String.concat "" ["Function(function"; " "; string_of_int line; ":"; string_of_int col; ")"]
      | End -> String.concat "" ["End(end"; " "; string_of_int line; ":"; string_of_int col; ")"]
      | Do -> String.concat "" ["Do(do"; " "; string_of_int line; ":"; string_of_int col; ")"]
      | And -> String.concat "" ["And(and"; " "; string_of_int line; ":"; string_of_int col; ")"]
      | Or -> String.concat "" ["Or(or"; " "; string_of_int line; ":"; string_of_int col; ")"]
      | Not -> String.concat "" ["Not(not"; " "; string_of_int line; ":"; string_of_int col; ")"]
      | For -> String.concat "" ["For(for"; " "; string_of_int line; ":"; string_of_int col; ")"]
      | While -> String.concat "" ["While(while"; " "; string_of_int line; ":"; string_of_int col; ")"]



module Token = struct 
  let make token_type lexeme line col = {token_type = token_type; lexeme = lexeme; line = line; col = col}
end