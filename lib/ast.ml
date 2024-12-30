type expr = 
  | Nil 
  | Int of int
  | Float of float
  | String of string
  | Boolean of bool

  | Less of expr * expr | Leq of expr * expr 
  | Greater of expr * expr | Geq of expr * expr 
  | Neq of expr * expr | Equal of expr * expr
  | And of expr * expr | Or of expr * expr

  | Add of expr * expr | Subtract of expr * expr
  | Multiply of expr * expr | Divide of expr * expr 
  | Power of expr * expr

  | Concat of expr * expr 

  | Not of expr | Negate of expr

  | Name of string

  | Grouping of expr

  | Function of  {
    name: string;
    params: string list;
    body: block;
  }

  | FunctionCall of fun_call

  | PrefixExpr of prefix_expr

  and fun_call = {
    target: prefix_expr;
    args: args;
  }
  
  and prefix_expr = 
    | Var of string
    | FunctionCall of fun_call
    | Grouping

  and args = 
    | ExprList of expr list
    | String of string

and stmt = 
  | AssignStmt of {
    ident: string;
    right: expr;
  }
  | FunctionCall of fun_call
  | DoBlock of block 
  | WhileLoop of {
    condition: expr;
    body: block
  }
  | FunctionDeclaration of {
    name: string;
    body: func_body;
  }
  | IfStmt of if_stmt
  | LastStmt of last_stmt

  and func_body = {
    params: string list;
    block: block
  }

  and if_stmt = {
    condition: expr;
    block: block;
    else_if: if_stmt option;
    else_block: block option;
  }

  and block = chunk 
  and chunk = {
    stmts: stmt list; 
    last_stmt: last_stmt option;
  }

  and last_stmt = 
    | ReturnStmt of expr option
    | Break 


let rec repeat_str str n = 
  if n = 0 then "" else str ^ (repeat_str str (n-1)) 


let stringify_lit = function 
  | String x -> "\""^x^"\""
  | Int x -> string_of_int x 
  | Float x -> string_of_float x
  | Boolean x -> if x = true then "true" else "false"
  | Nil -> "nil"
  | _ -> ""

    
let stringify_expr expr = 
  let rec stringify_expr expr level = 
    let indent = repeat_str "   " level in
    let stringify_bin_expr name l r  = 
      indent ^ name ^ ":\n" ^ indent ^ (stringify_expr l (level + 1)) ^ "\n" ^ indent ^ (stringify_expr r (level + 1)) 
    in
    match expr with 
      | Int x -> indent ^ "Int(" ^ string_of_int x ^ ")"
      | Float x -> indent ^ "Float(" ^ string_of_float x ^ ")"
      | String x -> indent ^ "String(\"" ^ x ^ "\")"
      | Negate x -> let str = (stringify_expr x level) in  
          let no_indent = Lexer.cut_first_n str (String.length indent) in
          let negated = "-" ^ no_indent in indent ^ negated
      | Grouping x -> indent ^ "(" ^ (stringify_expr x level) ^ ")"
      | Boolean x -> indent ^ if x then "true" else "false"
      | Name x -> indent ^ "Ident(\"" ^ x ^ "\")"
      
      | Greater (l, r) -> stringify_bin_expr "Greater" l r
      | Geq (l, r) -> stringify_bin_expr "Geq" l r
      | Less (l, r) -> stringify_bin_expr "Less" l r
      | Leq (l, r) -> stringify_bin_expr "Leq" l r
      | Equal (l, r) -> stringify_bin_expr "Equal" l r
      | Neq (l, r) -> stringify_bin_expr "Neq" l r

      | And (l, r) -> stringify_bin_expr "And" l r
      | Or (l, r) -> stringify_bin_expr "Or" l r

      | Add (l, r) -> stringify_bin_expr "Add" l r
      | Subtract (l, r) -> stringify_bin_expr "Subtract" l r
      | Multiply (l, r) -> stringify_bin_expr "Multiply" l r
      | Divide (l, r) -> stringify_bin_expr "Divide" l r
      | Concat (l, r) -> stringify_bin_expr "Concat" l r
      | Power (l, r) ->  stringify_bin_expr "Power" l r
      | _ -> ""
  in 
    stringify_expr expr 0


let stringify_stmt stmt = 
  match stmt with 
    | AssignStmt s -> "Assignment(\"" ^ s.ident ^ "\" = " ^ (stringify_expr s.right) ^ ")\n"
    | LastStmt ReturnStmt expr -> 
        let expr_string = match expr with 
          | Some e -> stringify_expr e
          | None -> "empty" in  
        "Return(" ^ expr_string ^ ")\n"
    | _ -> ""

let stringify_block block = 
  List.map stringify_stmt block.stmts