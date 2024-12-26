open Luacaml;;

let rec stringify_tokens = function 
| [] -> ""
| [x] -> Luacaml.Token.stringify_token x
| x :: xs -> Token.stringify_token x ^ ", " ^ stringify_tokens xs;;

let l: Lexer.lexer = Lexer.Tokenizer.create "356.78end" in
let lexer = Lexer.tokenize_number l in 
let token_str = stringify_tokens lexer.tokens in
print_string (token_str ^ "   " ^ lexer.current);;

print_string "\r\n";;

let l: Lexer.lexer = Lexer.Tokenizer.create "function" in
let lexer = Lexer.tokenize_ident l in 
let token_str = stringify_tokens lexer.tokens
in print_string token_str;;

print_string "\r\n";;

let tokens = Lexer.tokenize_source "567.12.5(){}[function\n\n\n\"hello, world\"end\"another one\"<>\n\n<=.==..~==^" in
let token_str = stringify_tokens tokens
in print_string token_str;;
