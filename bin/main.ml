open Luacaml;;

let load_source_file file_name = 
  let file = open_in file_name in 
  let rec load_source_file_aux file acc =
    try 
      let line = input_line file in 
        load_source_file_aux file (acc ^ line)
    with _ ->
      acc
  in 
    load_source_file_aux file "";;

let source = load_source_file "./test/test.calua" in

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