let is_digit = function '0' .. '9' -> true | _ -> false
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let cut_first_n str n = 
  let len = String.length str in 
    if len <= n then ""
    else String.sub str n (len - n) 

open Token

let keywords = Hashtbl.create 16;;

let fun_ident = Ident("function") in Hashtbl.add keywords "function" fun_ident;
let end_ident = Ident("end") in Hashtbl.add keywords "end" end_ident;
let do_ident = Ident("do") in Hashtbl.add keywords "end" do_ident;;

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
    let (str, updated_lexer) = tokenize_ident lexer "" in
    let token = {token_type = Ident str; lexeme = str; line = lexer.line; col = lexer.col} in
    let updated_tokens = token :: updated_lexer.tokens in
    Tokenizer.make lexer.source updated_lexer.current updated_lexer.line updated_lexer.col updated_tokens

let tokenize_source source = 
  let rec tokenize_source lexer = 
    let source_len = String.length lexer.current in
    if source_len = 0 then lexer 
    else
      let c = String.get lexer.current 0 in
      if is_digit c then 
        let lexer = tokenize_number lexer in tokenize_source lexer
      else if is_alpha c then
        let lexer = tokenize_ident lexer in tokenize_source lexer
      else 
        let lexer = tokenize_string {lexer with current = cut_first_n lexer.current 1}  in
          tokenize_source lexer
  in 
    let lexer = Tokenizer.create source in
    let processed_lexer = tokenize_source lexer in
    List.rev processed_lexer.tokens