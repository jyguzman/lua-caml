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
  | Concat of expr * expr 

  | Not of expr | Negate of expr
  | Pow of expr

  | Grouping of expr

let rec repeat_str str n = 
  if n = 0 then "" else str ^ (repeat_str str (n-1)) 
    
let stringify_expr expr = 
  let rec stringify_expr expr level = 
    let indent = repeat_str "   " level in
    match expr with 
      | Int x -> indent ^ "Int(" ^ string_of_int x ^ ")"
      | Float x -> indent ^ "Float(" ^ string_of_float x ^ ")"
      | String x -> indent ^ "\"" ^ x ^ "\""
      | Grouping x -> indent ^ "Grouping:\n" ^ (stringify_expr x (level))
      | Boolean x -> indent ^ if x then "true" else "false"
      | Add (l, r) -> indent ^ "Add:\n" ^ indent ^ (stringify_expr l (level + 1)) ^ "\n" ^ indent ^ (stringify_expr r (level + 1))
      | Subtract (l, r) -> indent ^ "Subtract:\n" ^ indent ^ (stringify_expr l (level + 1)) ^ "\n" ^ indent ^ (stringify_expr r (level + 1))
      | Multiply (l, r) -> indent ^ "Multiply:\n" ^ indent ^ (stringify_expr l (level + 1)) ^ "\n" ^ indent ^ (stringify_expr r (level + 1))
      | Divide (l, r) -> indent ^ "Divide:\n" ^ indent ^ (stringify_expr l (level + 1)) ^ "\n" ^ indent ^ (stringify_expr r (level + 1))
      | Concat (l, r) -> indent ^ "String:\n" ^ indent ^ (stringify_expr l (level + 1)) ^ "\n" ^ indent ^ (stringify_expr r (level + 1))
      | _ -> ""
  in 
    stringify_expr expr 0