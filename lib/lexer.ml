open Token;;

let cut_first_n str n = 
  let len = String.length str in 
    if len <= n then ""
    else String.sub str n (len - n) 

module Keywords = Map.Make(String);;

let keywords = Keywords.of_seq @@ List.to_seq [
  ("function", Keywords Function); ("return", Keywords Return);

  ("do", Keywords Do); ("end", Keywords End);

  ("and", Keywords And); ("or", Keywords Or); ("not", Keywords Not);

  ("in", Keywords In);

  ("for", Keywords For); ("while", Keywords While); 
  ("repeat", Keywords Repeat); ("until", Keywords Until);
  ("break", Keywords Break);

  ("if", Keywords If); ("else", Keywords Else); ("elseif", Keywords Elseif); ("then", Keywords Then);

  ("true", Keywords True); ("false", Keywords False);

  ("local", Keywords Local);
];;

type lexer = {
  source: string;
  current: string;
  line: int;
  col: int;
  tokens: token list;
}

module Tokenizer = struct
  exception InvalidToken of string
  exception UnterminatedString of string
  let raise_invalid_token lexeme line col = 
    let message = "Invalid token '" ^ lexeme ^ "' at line " ^ (string_of_int line) ^ ", col " ^ (string_of_int @@ col+1) in
    let exc = InvalidToken message in raise exc

  let raise_unterminated_str lexeme line col = 
    let message = "Unterminated string '" ^ lexeme ^ "' starting at line " ^ (string_of_int line) ^ ", col " ^ (string_of_int @@ col+1) in
    let exc = UnterminatedString message in raise exc
  let create source = {source = source; current = source; line = 1; col = 0; tokens = []}
  let make source current line col tokens = {source = source; current = current; line = line; col = col; tokens = tokens}
end

let peek lexer n = 
  let len = String.length lexer.current in
  if n >= len then '\x00'
  else String.get lexer.current n

let next lexer = peek lexer 1

let tokenize_string lexer = 
  let rec tokenize_string lexer acc =
    if String.length lexer.current = 0 then Tokenizer.raise_unterminated_str acc lexer.line lexer.col
    else
      let c = String.get lexer.current 0 in
      let c_str = String.make 1 c in
      let acc_new = acc ^ c_str in  
      let rest = cut_first_n lexer.current 1 in    
      if c = '"' then 
        let lexer = {lexer with current = cut_first_n lexer.current 1} in (acc, lexer)
      else 
        tokenize_string {lexer with current = rest; col = lexer.col + 1} acc_new
  in
    let str, updated_lexer = tokenize_string lexer "" in
    let token_type = String str in
    let token = Token.make "string" token_type str lexer.line lexer.col in
    let new_tokens = token :: updated_lexer.tokens in
    Tokenizer.make lexer.source updated_lexer.current updated_lexer.line updated_lexer.col new_tokens 
    

let tokenize_number lexer = 
  let rec tokenize_number lexer acc =
    if String.length lexer.current = 0 then acc, lexer
    else
      let c = String.get lexer.current 0 in
      let c_str = String.make 1 c in
      let acc_new = acc ^ c_str in  
      let rest = cut_first_n lexer.current 1 in 
      let updated_lexer = tokenize_number {lexer with current = rest; col = lexer.col + 1} acc_new in   
      match c with 
        | '0' .. '9' -> updated_lexer
        | '.' -> if String.contains acc '.' then (acc, lexer) else updated_lexer
        | _ -> acc, lexer
  in
    let num_string, updated_lexer = tokenize_number lexer "" in
    let name, token_type = 
      if String.contains num_string '.' then 
        "float", Float(float_of_string num_string)
      else 
        "integer", Integer(int_of_string num_string)
    in
    let token = Token.make name token_type num_string lexer.line lexer.col in
    let new_tokens = token :: updated_lexer.tokens in
    Tokenizer.make lexer.source updated_lexer.current updated_lexer.line updated_lexer.col new_tokens 

let tokenize_ident lexer = 
  let rec tokenize_ident lexer acc =
    if String.length lexer.current = 0 then acc, lexer
    else
      let c = String.get lexer.current 0 in
      let c_str = String.make 1 c in
      let rest = cut_first_n lexer.current 1 in
      let acc_new = acc ^ c_str in match c with 
        | 'a' .. 'z' | 'A' .. 'Z' | '_' -> tokenize_ident {lexer with current = rest; col = lexer.col + 1} acc_new 
        | _ -> acc, lexer
  in
    let ident, updated_lexer = tokenize_ident lexer "" in
    let keyword = Keywords.find_opt ident keywords in 
    let name, token_type = match keyword with 
      | Some keyword_type -> ident, keyword_type
      | None -> "ident", Ident ident 
    in
    let token = Token.make name token_type ident lexer.line lexer.col in
    let updated_tokens = token :: updated_lexer.tokens in
    Tokenizer.make lexer.source updated_lexer.current updated_lexer.line updated_lexer.col updated_tokens

let tokenize_char lexer c = 
  let next = peek lexer 1 in 
  let name, op, lexeme = match c with
    | '-' -> ("minus", Minus, "-") | '^' -> ("caret", Caret, "^")
    
    | '*' -> ("star", BinOp Star, "*") | '+' -> ("plus", BinOp Plus, "+")
    | '/' -> ("slash", BinOp Slash, "/")
    | '<' -> if next = '=' then ("leq", BinOp Leq, "<=") else ("less", BinOp Less, "<")
    | '>' -> if next = '=' then ("geq", BinOp Geq, ">=") else ("greater", BinOp Greater, ">")
    | '=' -> if next = '=' then ("equal", BinOp Equal, "==") else ("assign", BinOp Assign, "=")
    | '~' -> if next = '=' then ("notequal", BinOp NotEqual, "~=") 
      else Tokenizer.raise_invalid_token "~" lexer.line lexer.col
    | '.' -> if next = '.' then ("dotdot", BinOp DotDot, "..") else ("dot", BinOp Dot, ".")

    | '[' -> ("lbracket", Punctuation LBracket, "[") | ']' -> ("rbracket", Punctuation RBracket, "]")
    | '{' -> ("lbrace", Punctuation LBrace, "{") | '}' -> ("rbrace", Punctuation RBrace, "}")
    | '(' -> ("lparen", Punctuation LParen, "(") | ')' -> ("rparen", Punctuation RParen, ")")
    | ';' -> ("semicolon", Punctuation Semicolon, ";") | ':' -> ("colon", Punctuation Colon, ":")

    | _ -> Tokenizer.raise_invalid_token (String.make 1 c) lexer.line lexer.col
  in 
  let token = Token.make name op lexeme lexer.line lexer.col in
  let n = String.length token.lexeme in 
  let skip = cut_first_n lexer.current n in 
  {lexer with col = lexer.col + n; current = skip; tokens = token :: lexer.tokens}
       
let tokenize_source source = 
  let rec tokenize_source lexer = 
    if String.length lexer.current = 0 then 
      let eof = Token.make "eof" EOF "" lexer.line lexer.col in {lexer with tokens = eof :: lexer.tokens} 
    else
      let c = peek lexer 0 in
      let skip = cut_first_n lexer.current 1 in 
      tokenize_source @@ match c with 
        | '0' .. '9' -> tokenize_number lexer 

        | 'a' .. 'z' | 'A' .. 'Z' -> tokenize_ident lexer 

        | '"' -> tokenize_string {lexer with current = skip} 

        | '^' | '<' | '>' | '=' | '~' | '+' | '-' | '/' | '*' |'.' 
        | '[' | ']' | '{' | '}' | '(' | ')' -> tokenize_char lexer c

        | '\n' -> {lexer with line = lexer.line + 1; col = 0; current = skip}

        | ' ' -> {lexer with col = lexer.col + 1; current = skip}
        
        | _ -> Tokenizer.raise_invalid_token (String.make 1 c) lexer.line lexer.col
  in 
    let new_lexer = Tokenizer.create source in
    let processed_lexer = tokenize_source new_lexer in
    List.rev processed_lexer.tokens