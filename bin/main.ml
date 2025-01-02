open Luacaml;;
let source = 
{|
x = 10 
while x > 0 do 
  if x == 3 then 
    print ("I'm 3 now!") 
  elseif x == 6 then 
    print ("I'm 6 now!")
  elseif x == 9 then 
    print("I'm 9 now!")
  else 
    print ("I'm not 3 or 6!")
  end
  x = x - 1
end
|} 

in 

let tokens = Lexer.tokenize_source source in 
let prog = Parser.parse_program tokens in
match prog with 
| Ok (prog, _) -> 
    let _ = print_string ("Program AST:\n" ^ Ast.stringify_block prog ^ "\n\n") in 
    (match Eval.eval_program prog with 
    | Ok () -> Ok ()
    | Error e -> raise e)
| Error e -> raise e
;;