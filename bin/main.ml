let (token, rest) = Luacaml.Lexer.tokenize_number "356.78abf" in 
let token_str = Luacaml.Token.stringify_token token in
print_string (token_str ^ rest);;

print_string "\r\n";;

let (token, rest) = Luacaml.Lexer.tokenize_ident "bubg7.8" in 
let token_str = Luacaml.Token.stringify_token token
in print_string (token_str ^ rest);;

print_string "\r\n";;

let rec stringify_tokens = function 
  | [] -> ""
  | [x] -> Luacaml.Token.stringify_token x
  | x :: xs -> Luacaml.Token.stringify_token x ^ ", " ^ stringify_tokens xs
in
let tokens = Luacaml.Lexer.tokenize_source "567.12bubg6.78\"helloworld\"" in
let token_str = stringify_tokens tokens
in print_string token_str;;
