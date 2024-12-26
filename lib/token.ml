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
  | Percent 

type keywords = 
  | Function 
  | Do | End | Return
  | Not | Or | And
  | For | While | Repeat | Until | Break
  | If | Else | Elseif | Then
  | True | False
  | Nil  
  | Local
  | In

type punctuation = 
  | LBrace | RBrace 
  | LBracket | RBracket 
  | LParen | RParen 
  | Semicolon | Comma | Colon


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
  | Punctuation of punctuation

type token = {
  name: string;
  token_type: token_type;
  lexeme: string;
  line: int;
  col: int
}

let stringify_token token = 
  if token.token_type = Dummy then ""
  else String.concat "" [
    String.uppercase_ascii token.name; "("; 
    token.lexeme ^ " "; 
    string_of_int token.line ^ ":" ^ string_of_int token.col; ")"
  ]



module Token = struct 
  let make name token_type lexeme line col = {name = name; token_type = token_type; lexeme = lexeme; line = line; col = col}
end