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

  | Name of name

  | Grouping of expr

  | Function of func

  | FunctionCall of fun_call

  | PrefixExpr of prefix_expr

  and name = string
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
  | Function of func
  | IfStmt of if_stmt
  | LastStmt of last_stmt

  and func = {
    name: name;
    body: func_body
  }
  
  and func_body = {
    params: expr list;
    block: block
  }

  and if_stmt = {
    condition: expr;
    then_block: block;
    elseif: if_stmt option;
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
      name ^ "(" ^ (stringify_expr l (level + 1)) ^ ", " ^ (stringify_expr r (level + 1)) ^ ")"
    in
    match expr with 
      | Int x -> "Int(" ^ string_of_int x ^ ")"
      | Float x -> "Float(" ^ string_of_float x ^ ")"
      | String x -> "String(\"" ^ x ^ "\")"
      | Negate x -> let str = (stringify_expr x level) in  
          let no_indent = Lexer.cut_first_n str (String.length indent) in
          let negated = "-" ^ no_indent in indent ^ negated
      | Grouping x -> "Grouping(" ^ (stringify_expr x level) ^ ")"
      | Boolean x -> "Boolean(" ^ if x then "true" else "false" ^ ")"
      | Name x -> "Ident(\"" ^ x ^ "\")"
      
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

let empty_block = {stmts=[]; last_stmt = None}
let empty_if = IfStmt{condition = Nil; then_block = empty_block; elseif = None; else_block = None}

let make_optional block = 
  if List.length block.stmts = 0 && block.last_stmt = None then None
  else Some block

let make_optional_if if_stmt =
  match if_stmt with 
    | None -> None
    | Some f -> 
      match f with
        f when f = {condition = Nil; 
          then_block = {stmts=[]; last_stmt = None}; 
          elseif = None; 
          else_block = None} -> None 
      | _ -> Some f

let stmt_to_if stmt = match stmt with IfStmt f -> Some f | _ -> None
let stmt_to_func stmt = match stmt with Function f -> Some f | _ -> None

let rec stringify_block block = 
  let ret_stmt_str = [match block.last_stmt with None -> "" | Some ret -> stringify_stmt (LastStmt ret)] in
  let stmt_strings = List.map stringify_stmt block.stmts in 
  "[" ^ String.concat "," (List.rev_append stmt_strings ret_stmt_str) ^ "]"

and stringify_function func = 
  let name, params, block = func.name, func.body.params, func.body.block in
  let params_str = List.fold_left (fun acc x -> acc ^ ", " ^ x) "" (List.map stringify_expr params) in
  let block_str = stringify_block block in  
  name ^ "(params: " ^ params_str ^ "body: " ^ block_str ^ ")"

and stringify_stmt stmt = 
  match stmt with 
    | AssignStmt s -> "Assignment(\"" ^ s.ident ^ "\" = " ^ (stringify_expr s.right) ^ ")"
    | LastStmt ReturnStmt expr -> 
        let expr_string = match expr with 
          | Some e -> stringify_expr e
          | None -> "empty" in  
        "Return(" ^ expr_string ^ ")\n"
    | LastStmt Break -> "Break()"
    | WhileLoop wl -> "While(condition = " ^ stringify_expr wl.condition ^ "\n block = " ^ stringify_block wl.body ^ ")"
    | IfStmt i -> stringify_if_stmt i
    | Function f -> stringify_function f
    | _ -> ""

and stringify_if_stmt_aux if_stmt level = 
  let indent = repeat_str "\t" (level + 1) in 
  let then_str = "\n" ^ indent ^ "then: " ^ stringify_block if_stmt.then_block in 
  let else_str = match if_stmt.else_block with None -> "" | Some e -> "\n" ^ indent ^ "else: " ^ stringify_block e in
  let else_if_str = match if_stmt.elseif with None -> "" | Some e -> "\n" ^ indent ^ "else if:\n " ^ (stringify_if_stmt_aux e (level + 1)) in
  indent ^ "IF(\n" ^ indent ^ "condition = " ^ stringify_expr if_stmt.condition ^ indent ^ then_str ^ else_if_str ^ else_str ^ "\nEND)"
and stringify_if_stmt if_stmt =
  stringify_if_stmt_aux if_stmt 0


