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

type annotation = 
  | Number 
  | Bool 
  | TString
  | TNil
  | Table of annotation * annotation 

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
  | TIdent
  | Caret
  | Minus
  | BinOp of bin_op_token_type
  | Keywords of keywords
  | Punctuation of punctuation
  | EOF

let get_tok_type token_type = match token_type with 
  | Ident _ -> TIdent
  | _ -> token_type
  
type token = {
  name: string;
  token_type: token_type;
  lexeme: string;
  line: int;
  col: int
}

let stringify_token token = String.concat "" [
  String.uppercase_ascii token.name; "(\""; 
  token.lexeme ^ "\" line "; 
  string_of_int token.line ^ ", col " ^ string_of_int token.col; ")"
]

let rec stringify_tokens = function 
  | [] -> ""
  | [x] -> stringify_token x
  | x :: xs -> stringify_token x ^ ", " ^ stringify_tokens xs;;

module Token = struct 
  let make name token_type lexeme line col = {name = name; token_type = token_type; lexeme = lexeme; line = line; col = col}
end