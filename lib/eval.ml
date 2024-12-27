open Ast;;

let rec eval_expr expr = 
  match expr with 
    | Int x -> x
    | Add (l, r) -> 
      let left = eval_expr l in 
      let right = eval_expr r in 
      left + right
    | Multiply (l, r) -> 
      let left = eval_expr l in 
      let right = eval_expr r in 
      left * right
    | _ -> 0