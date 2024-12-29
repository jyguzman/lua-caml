open Luacaml;;

(* let source = "
  function do_something(x: number): number 
    if x < 10 then
      return \"I'm less than 10!\"
    elseif x == 10 then 
      y: int = 20
      return y
    else
      return x * x
    end
  end
";;

let tokens = Lexer.tokenize_source source in
let token_str = Token.stringify_tokens tokens
in print_string token_str;; *)

(* let expr = 
  Ast.Add(
    Ast.Int 3, 
    Ast.Multiply(
      Ast.Int 10, 
      Ast.Subtract(
        Ast.Float 5.5,
        Ast.Int 50
      )
    )
  ) in 
let expr_string = Ast.stringify_expr expr in 
print_string expr_string;;  *)

(* let source = "2 ^ (3*(-5^-2+8-9*-2-2-2- -2) -2 ^ -3 - (-3-(4)+(9-2+4^(-2))))" in  *)
let source = "x = 5 y = 10 z = 20 return x + y + z" in
let tokens = Lexer.tokenize_source source in 
(* let tok_string = (Token.stringify_tokens tokens ^ "\r\n") in 
let _ = print_string tok_string in *)
(* let parser = ExpressionParser.make tokens in  
let ast = ExpressionParser.parse_exp parser in 
let ast_string = Ast.stringify_expr ast in *)
(* let ast = Parser.parse_expr__ tokens in
let ast_string = Ast.stringify_expr ast in 
let _ = print_string ("tree: \n" ^ ast_string ^ "\n") in 
let ans = Luacaml.Eval.eval_expr ast in
let _ = print_string ("eval result: \n" ^ Ast.stringify_expr ans) in *)

(* let stmt_res = Parser.parse_stmt tokens in 
match stmt_res with 
  | Ok (stmt, _) -> print_string ("stmt: " ^ Ast.stringify_stmt stmt) 
  | Error e -> raise e *)

let block_res = Parser.parse_block tokens in 
match block_res with 
  | Ok (block, _) -> List.iter print_string (Ast.stringify_block block) 
  | Error e -> raise e
;;