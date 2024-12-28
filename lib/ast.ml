type expr = 
  | NilExp 
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

let rec repeat_str str n = 
  if n = 0 then "" else str ^ (repeat_str str (n-1)) 
    
let stringify_expr expr = 
  let rec stringify_expr expr level = 
    let indent = repeat_str "   " level in
    let stringify_bin_expr name ident level l r  = 
      indent ^ name ^ ":\n" ^ ident ^ (stringify_expr l (level + 1)) ^ "\n" ^ indent ^ (stringify_expr r (level + 1)) 
    in
    match expr with 
      | Int x -> indent ^ "Int(" ^ string_of_int x ^ ")"
      | Float x -> indent ^ "Float(" ^ string_of_float x ^ ")"
      | String x -> indent ^ "\"" ^ x ^ "\""
      | Negate x -> indent ^ "-" ^ (stringify_expr x 0) 
      | Grouping x -> indent ^ "Grouping:\n" ^ (stringify_expr x 0)
      | Boolean x -> indent ^ if x then "true" else "false"
      
      | Greater (l, r) -> stringify_bin_expr "Greater" indent level l r
      | Geq (l, r) -> stringify_bin_expr "Geq" indent level l r
      | Less (l, r) -> stringify_bin_expr "Less" indent level l r
      | Leq (l, r) -> stringify_bin_expr "Leq" indent level l r
      | Equal (l, r) -> stringify_bin_expr "Equal" indent level l r
      | Neq (l, r) -> stringify_bin_expr "Neq" indent level l r

      | And (l, r) -> stringify_bin_expr "And" indent level l r
      | Or (l, r) -> stringify_bin_expr "Or" indent level l r

      | Add (l, r) -> stringify_bin_expr "Add" indent level l r
      | Subtract (l, r) -> stringify_bin_expr "Subtract" indent level l r
      | Multiply (l, r) -> stringify_bin_expr "Multiply" indent level l r
      | Divide (l, r) -> stringify_bin_expr "Divide" indent level l r
      | Concat (l, r) -> stringify_bin_expr "Concat" indent level l r
      | Power (l, r) -> stringify_bin_expr "Power" indent level l r
      | _ -> ""
  in 
    stringify_expr expr 0