open Ast;;

(* let rec eval_int_or_float op = match op with 
  | (+) *)


let rec eval_expr expr = 
  match expr with 
    | Int x -> Float(float_of_int x)
    | Float x -> Float(x)
    | Negate x -> eval_expr (Multiply(Float(-1.0), eval_expr x))
    | Power (l, r) -> 
      let left = eval_expr l in let right = eval_expr r in 
        (match left, right with 
          | Float x, Float y  -> Float(x ** y)
          | Float x, Int y -> Float(x ** float_of_int y)
          | Int x, Float y -> Float(float_of_int x ** y)
          | Int x, Int y -> Int(int_of_float (float_of_int x ** float_of_int y))
          | _ -> Float(0.0))
    | Add (l, r) -> 
      let left = eval_expr l in let right = eval_expr r in 
        (match left, right with 
          | Float x, Float y  -> Float(x +. y)
          | Float x, Int y -> Float(x +. float_of_int y)
          | Int x, Float y -> Float(float_of_int x +. y)
          | Int x, Int y -> Float(float_of_int x +. float_of_int y)
          | _ -> Float(0.0))
    | Subtract (l, r) -> 
      let left = eval_expr l in let right = eval_expr r in 
        (match left, right with 
          | Float x, Float y  -> Float(x -. y)
          | Float x, Int y -> Float(x -. float_of_int y)
          | Int x, Float y -> Float(float_of_int x -. y)
          | Int x, Int y -> Float(float_of_int x -. float_of_int y)
          | _ -> Float(0.0))
    | Multiply (l, r) -> 
      let left = eval_expr l in let right = eval_expr r in 
        (match left, right with 
          | Float x, Float y  -> Float(x *. y)
          | Float x, Int y -> Float(x *. float_of_int y)
          | Int x, Float y -> Float(float_of_int x *. y)
          | Int x, Int y -> Float(float_of_int x *. float_of_int y)
          | _ -> Float(0.0))
    | _ -> Float(0.0)