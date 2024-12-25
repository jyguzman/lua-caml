let is_digit = function '0' .. '9' -> true | _ -> false
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let cut_first_n str n = 
  let len = String.length str in 
    if len <= n then ""
    else String.sub str n (len - n) 

open Token

let keywords = Hashtbl.create 16;;

let fun_ident = Ident("function") in Hashtbl.add keywords "function" fun_ident;
let end_ident = Ident("end") in Hashtbl.add keywords "end" end_ident;;


let tokenize_number source = 
  let rec tokenize_number source num_string_acc =
    let source_len = String.length source in
    if source_len = 0 then (num_string_acc, source)
    else
      let c = String.get source 0 in 
      let c_str = String.make 1 c in
      let rest = cut_first_n source 1 in
      let acc_new = num_string_acc ^ c_str in 
      if is_digit c then 
        tokenize_number rest acc_new
      else if c = '.' then
        tokenize_number rest acc_new
      else (num_string_acc, source)
  in
  let open Token in
  let (num_string, remaining) = tokenize_number source ""  in  
  let token_type =
      if String.contains num_string '.'
        then Float(float_of_string num_string)
      else 
        Integer(int_of_string num_string)
    in ({token_type = token_type; 
      lexeme = num_string}, remaining)

let tokenize_string source = 
  let rec tokenize_string source acc =
    let source_len = String.length source in
    if source_len = 0 then (acc, source)
    else
      let c = String.get source 0 in
      let c_str = String.make 1 c in
      let acc_new = acc ^ c_str in  
      let rest = cut_first_n source 1 in    
      if c = '"' then let skip_quote = cut_first_n source 1 in (acc, skip_quote) 
      else tokenize_string rest acc_new
  in
    let (str, rest) = tokenize_string source "" in
    ({token_type = String str; lexeme = str}, rest)

let tokenize_ident source = 
  let rec tokenize_ident source acc =
    let source_len = String.length source in
    if source_len = 0 then (acc, source)
    else
      let c = String.get source 0 in
      let c_str = String.make 1 c in
      let rest = cut_first_n source 1 in
      let acc_new = acc ^ c_str in      
      if not (is_alpha c) then (acc, source) 
      else tokenize_ident rest acc_new
  in
    let open Token in
    let (str, remaining) = tokenize_ident source "" in
    ({token_type = Ident str; lexeme = str}, remaining)

let tokenize_source source = 
  let rec tokenize_source source tokens = 
    let source_len = String.length source in
    if source_len = 0 then tokens 
    else
      let c = String.get source 0 in
      if is_digit c then 
        let (token, rest) = tokenize_number source in 
        let new_tokens = (token :: tokens) in
        tokenize_source rest new_tokens
      else if is_alpha c then
        let (token, rest) = tokenize_ident source in 
        let new_tokens = (token :: tokens) in
        tokenize_source rest new_tokens
      else 
        let no_first_quote = cut_first_n source 1 in
        let (token, rest) = tokenize_string no_first_quote in 
        let new_tokens = (token :: tokens) in
        tokenize_source rest new_tokens
  in 
    let tokens = tokenize_source source [] in
    List.rev tokens