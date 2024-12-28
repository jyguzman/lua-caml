open Ast;;

exception InvalidOperation of string;;

let raise_invalid_op msg = raise (InvalidOperation msg)

let rec eval_expr expr = 
  match expr with
    | Int _ | Float _ | String _ | Boolean _ | Nil -> expr 
    | Concat (l, r) -> 
      let left = eval_expr l in let right = eval_expr r in 
        (match left, right with 
          | String x, String y -> String(String.cat x y)
          | String x, _ -> raise_invalid_op ("Cannot concatenate string \"" ^ x ^ "\" with " ^ stringify_lit right)
          | _, String y -> raise_invalid_op ("Cannot concatenate string \"" ^ y ^ "\" with " ^ stringify_lit left)
          | _ -> raise_invalid_op "Can only concatenate strings")
    | Negate x -> 
      eval_expr (Multiply(Float(-1.0), eval_expr x))
    | Power (l, r) -> 
      let left = eval_expr l in let right = eval_expr r in 
        (match left, right with 
          | Float x, Float y  -> Float(x ** y)
          | Float x, Int y -> Float(x ** float_of_int y)
          | Int x, Float y -> Float(float_of_int x ** y)
          | Int x, Int y -> Int(int_of_float (float_of_int x ** float_of_int y))
          | _ -> raise_invalid_op ("Invalid exponentation between " ^ stringify_lit left ^ " and " ^ stringify_lit right))
    | Add (l, r) -> 
      let left = eval_expr l in let right = eval_expr r in 
        (match left, right with 
          | Float x, Float y  -> Float(x +. y)
          | Float x, Int y -> Float(x +. float_of_int y)
          | Int x, Float y -> Float(float_of_int x +. y)
          | Int x, Int y -> Int(int_of_float (float_of_int x +. float_of_int y))
          | _ -> Float(0.0))
    | Subtract (l, r) -> 
      let left = eval_expr l in let right = eval_expr r in 
        (match left, right with 
          | Float x, Float y  -> Float(x -. y)
          | Float x, Int y -> Float(x -. float_of_int y)
          | Int x, Float y -> Float(float_of_int x -. y)
          | Int x, Int y -> Int(int_of_float (float_of_int x -. float_of_int y))
          | _ -> Float(0.0))
    | Multiply (l, r) -> 
      let left = eval_expr l in let right = eval_expr r in 
        (match left, right with 
          | Float x, Float y  -> Float(x *. y)
          | Float x, Int y -> Float(x *. float_of_int y)
          | Int x, Float y -> Float(float_of_int x *. y)
          | Int x, Int y -> Int(int_of_float (float_of_int x *. float_of_int y))
          | _ -> Float(0.0))
    | _ -> Float(0.0)