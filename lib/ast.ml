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

  | Grouping of expr

  | Var of var
  | Name of name
  | Function of func_body

  and func_body = {
    params: param_list;
    body: block;
  }

  and name = string
  and var =
    | Name
    | IndexExpr of {
        target: prefix_expr;
        subscript: expr;
      } 
    | PropertyAccessExpr of {
        target: prefix_expr;
        property: expr;
      }
  
  and prefix_expr = 
    | Var 
    | FunctionCall of fun_call_expr 
    | Grouping

  and fun_call_expr = 
    | DirectFunctionCall of {
      target: prefix_expr;
      args: args;
    }
    | MethodCall of {
      target: prefix_expr;
      target_name: name;
    }

  and args = 
    | ExprList of {exprs: expr list}
    | TableConstructor of table_constructor
    | String 

  and table_constructor = 
    | FieldList of {fields: field list;}

  and field =
    | Expr of expr 
    | KeyValStmt of {
      key: expr;
      value: expr;
    }
    | NameKeyStmt of {
      name: name;
      value: expr;
    }

and stmt = 
  | AssignStmt of {
    var_list: var list;
    exp_list: expr list;
  }
  | FunctionCall of fun_call_expr
  | DoBlock of block 
  | WhileLoop of {
    condition: expr;
    body: block
  }
  | RepeatLoop of {
    body: block;
    condition: expr;
  }
  | IfStmt of if_stmt
  | ForLoop of {
    name: expr;
    block: block;
  }
  | ForNamelist
  | FunctionDeclaration of {
    name: function_name;
    body: func_body;
  }

  and function_name = 
    | Name 

  and if_stmt = {
    condition: expr;
    block: block;
    else_if: if_stmt option;
    else_block: block option;
  }


  and block = chunk 
  and chunk = {
    stmts: stmt list; 
    last_stmt: last_stmt
  }

  and last_stmt = 
    | ReturnStmt of expr list
    | Break 

  and param_list = {
    names: name_list;
  }

  and name_list = name list


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