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
(* let source = "x = 50 y = 100 z = 100 return x + y" in *)
let source = 
  "while x == 50 do
    if x ~= 50 then 
      if disney == good then 
        boss = \"bossman\"
      else 
        boss_two = \"miniboss\"
      end
      break
    end
    while thing == none do 
      this = that
      while bong == bing do 
        bangbang = 1000
      end
    end
  end" in
(* let source = 
  "if thing == thong then 
    first = one
  elseif fatoi == 10000 then 
    if innerif ~= outies then 
      thisiswerod = blamblam
    else
      jfskj = sdjdsdskjdskj
    end
    second = two
  else
    third = three
  end" in *)
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

(* let params_res = Parser.parse_params tokens in 
match params_res with 
  Ok (params, _) -> 
    let strings = List.map Ast.stringify_expr params
      in List.iter print_string (List.rev strings)
  | Error e -> raise e *)

let block_res = Parser.parse_block tokens in
match block_res with 
  | Ok (block, _) -> print_string ("reaaal block:\n\n\n\n" ^ Ast.stringify_block block) 
  | Error e -> raise e
;;