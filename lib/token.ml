type bin_op_token_type = 
  | Less 
  | Leq
  | Greater
  | Geq
  | Equal

type token_type = 
  | Integer of int
  | Float of float
  | String of string
  | Ident of string
  | Comma
  (*BinOp of bin_op_token_type*)

type token = {
  token_type: token_type;
  lexeme: string;
  line: int;
  col: int
}

let stringify_token token = 
  let col = token.col in 
  let line = token.line in
    match token.token_type with 
    | Integer(x) -> String.concat "" ["Integer("; string_of_int x; " "; string_of_int line; ":"; string_of_int col; ")"]
    | Float(x) -> String.concat "" ["Float("; string_of_float x; " "; string_of_int line; ":"; string_of_int col; ")"]
    | String(x) -> String.concat "" ["String("; x; " "; string_of_int line; ":"; string_of_int col; ")"]
    | Ident(x) -> String.concat "" ["Ident("; x; " "; string_of_int line; ":"; string_of_int col; ")"]
    | Comma -> "Comma(,)"
    (* | BinOp(x) -> match x with 
      | Leq *)