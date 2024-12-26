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

let source = "
  function do_something(x) do 
    if x < 10 then
      return \"I'm less than 10!\"
    elseif x == 10 then 
      local y = 20
      return y
    else
      return x * x
    end
  end
";;

let tokens = Lexer.tokenize_source source in
let token_str = stringify_tokens tokens
in print_string token_str;;
