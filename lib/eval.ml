open Ast;;

let rec eval_expr expr = 
  match expr with 
    | Int x -> x
    | Grouping x -> eval_expr x
    | Negate x -> -1 * (eval_expr x)
    | Add (l, r) -> 
      let left = eval_expr l in 
      let right = eval_expr r in 
      left + right
    | Subtract (l, r) -> 
      let left = eval_expr l in 
      let right = eval_expr r in 
      left - right
    | Multiply (l, r) -> 
      let left = eval_expr l in 
      let right = eval_expr r in 
      left * right
    | _ -> 0