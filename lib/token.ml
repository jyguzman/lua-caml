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

type token = {
  token_type: token_type;
  lexeme: string;
}

let stringify_token token = match token.token_type with 
  | Integer(x) -> String.concat "" ["Token("; string_of_int x; ")"]
  | Float(x) -> String.concat "" ["Token("; string_of_float x; ")"]
  | String(x) -> String.concat "" ["Token(\""; x; "\")"]
  | Ident(x) -> String.concat "" ["Token("; x; ")"]
  | Comma -> "Token(,)"