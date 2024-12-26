let is_digit = function '0' .. '9' -> true | _ -> false
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let cut_first_n str n = 
  let len = String.length str in 
    if len <= n then ""
    else String.sub str n (len - n) 

open Token

module Keywords = Map.Make(String);;

let keywords = Keywords.of_seq @@ List.to_seq [
  ("function", Keywords Function);
  ("end", Keywords End);
  ("do", Keywords Do);
  ("not", Keywords Not);
  ("or", Keywords Or);
  ("and", Keywords And);
  ("for", Keywords For);
  ("while", Keywords While);
];;

type lexer = {
  source: string;
  current: string;
  line: int;
  col: int;
  tokens: token list;
}

module Tokenizer = struct
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
    let source_len = String.length lexer.current in
    if source_len = 0 then (acc, lexer)
    else
      let c = String.get lexer.current 0 in
      let c_str = String.make 1 c in
      let acc_new = acc ^ c_str in  
      let rest = cut_first_n lexer.current 1 in    
      if c = '"' then 
        let lexer = {lexer with current = cut_first_n lexer.current 1} in 
        (acc, lexer)
      else 
        tokenize_string {lexer with current = rest; col = lexer.col + 1} acc_new
  in
    let (str, updated_lexer) = tokenize_string lexer "" in
    let token = {token_type = String str; lexeme = str; line = updated_lexer.line; col = lexer.col} in
    let new_tokens = token :: updated_lexer.tokens in
    Tokenizer.make lexer.source updated_lexer.current updated_lexer.line updated_lexer.col new_tokens 
    

let tokenize_number lexer = 
  let rec tokenize_number lexer acc =
    let source_len = String.length lexer.current in
    if source_len = 0 then (acc, lexer)
    else
      let c = String.get lexer.current 0 in
      let c_str = String.make 1 c in
      let acc_new = acc ^ c_str in  
      let rest = cut_first_n lexer.current 1 in    
      if is_alpha c 
        then (acc, lexer)
      else 
          tokenize_number {lexer with current = rest; col = lexer.col + 1} acc_new
  in
    let (num_string, updated_lexer) = tokenize_number lexer "" in
    let token_type = 
      if String.contains num_string '.' 
        then Float(float_of_string num_string)
      else
        Integer(int_of_string num_string)
      in
    let token = {token_type = token_type; lexeme = num_string; line = lexer.line; col = lexer.col} in
    let new_tokens = token :: updated_lexer.tokens in
    Tokenizer.make lexer.source updated_lexer.current updated_lexer.line updated_lexer.col new_tokens 

let tokenize_ident lexer = 
  let rec tokenize_ident lexer acc =
    let source_len = String.length lexer.current in
    if source_len = 0 then (acc, lexer)
    else
      let c = String.get lexer.current 0 in
      let c_str = String.make 1 c in
      let rest = cut_first_n lexer.current 1 in
      let acc_new = acc ^ c_str in      
      if is_alpha c then 
        tokenize_ident {lexer with current = rest; col = lexer.col + 1} acc_new 
      else
        (acc, lexer) 
  in
    let (ident, updated_lexer) = tokenize_ident lexer "" in
    let keyword = Keywords.find_opt ident keywords in 
    let token_type = match keyword with Some t -> t | None -> Ident ident in
    let token = {token_type = token_type; lexeme = ident; line = lexer.line; col = lexer.col} in
    let updated_tokens = token :: updated_lexer.tokens in
    Tokenizer.make lexer.source updated_lexer.current updated_lexer.line updated_lexer.col updated_tokens

let tokenize_op lexer c = 
  let next = peek lexer 1 in 
  let dummy = (Dummy, "") in
  let (op, lexeme) = match c with
  | '-' -> (Minus, "-") | '^' -> (Caret, "^")
  | '*' -> (BinOp Star, "*")
  | '<' -> if next = '=' then (BinOp Leq, "<=") else (BinOp Less, "<")
  | '>' -> if next = '=' then (BinOp Geq, ">=") else (BinOp Greater, ">")
  | '=' -> if next = '=' then (BinOp Equal, "==") else (BinOp Assign, "=")
  | '~' -> if next = '=' then (BinOp NotEqual, "~=") else dummy
  | '.' -> if next = '.' then (BinOp DotDot, "..") else (BinOp Dot, ".")
  | _ -> dummy
  in 
  if op == Dummy then lexer 
  else
    let token = Token.make op lexeme lexer.line lexer.col in
    let n = String.length token.lexeme in 
    let skip = cut_first_n lexer.current n in 
    {lexer with col = lexer.col + n; current = skip; tokens = token :: lexer.tokens}
       

let tokenize_source source = 
  let rec tokenize_source lexer = 
    let source_len = String.length lexer.current in
    if source_len = 0 then lexer 
    else
      let c = String.get lexer.current 0 in
      let skip = cut_first_n lexer.current 1 in 
      let updated_lexer = match c with 
        | '0' .. '9' -> tokenize_number lexer 
        | 'a' .. 'z' | 'A' .. 'Z' -> tokenize_ident lexer 
        | '"' -> tokenize_string {lexer with current = skip} 
        | '^' | '<' | '>' | '=' | '~' | '+' | '-' | '/' | '*' |'.' -> tokenize_op lexer c
        | '\n' -> {lexer with line = lexer.line + 1; col = 0; current = skip}
        | _ -> {lexer with col = lexer.col + 1; current = skip}    
      in tokenize_source updated_lexer
  in 
    let new_lexer = Tokenizer.create source in
    let processed_lexer = tokenize_source new_lexer in
    List.rev processed_lexer.tokens