open Ast;;
open Env;; 
exception InvalidOperation of string;;

let ( let* ) r f = match r with 
  Ok v -> f v 
| Error e -> Error e

let raise_invalid_op msg = raise (InvalidOperation msg)

let rec eval_expr envs (expr: expr) = match expr with
    | Name x -> 
      let result = Env.resolve envs x in
      (match result with 
        Ok (res) -> Ok(res)
        | Error e -> Error e)

    | Int _ | Float _ | String _ | Boolean _ | Nil | Function _ -> Ok(expr)
    | Grouping x -> eval_expr envs x

    (* | Concat (l, r) -> 
      let *left = eval_expr envs l in 
      let *right = eval_expr envs r in 
        (match left, right with 
          | String x, String y -> String(String.cat x y)
          | String x, _ -> Error (InvalidOperation ("Cannot concatenate string \"" ^ x ^ "\" with " ^ stringify_lit right))
          | _, String y -> Error (InvalidOperation ("Cannot concatenate string \"" ^ y ^ "\" with " ^ stringify_lit left))
          | _ -> Error (InvalidOperation "Can only concatenate strings"))  *)
    
    | Negate x -> let* y = eval_expr envs x in 
      (match y with 
        | Int _ -> eval_expr envs (Multiply(Int(-1), y))
        | Float _ -> eval_expr envs (Multiply(Float(-1.0), y))
        | _ -> Error (InvalidOperation ("Cannot negate" ^ stringify_lit x)))

    | Power (l, r) -> 
      let* left = eval_expr envs l in 
      let* right = eval_expr envs r in 
        (match left, right with 
        | Float x, Float y  -> Ok (Float(x ** y))
        | Float x, Int y -> Ok (Float(x ** float_of_int y))
        | Int x, Float y -> Ok (Float(float_of_int x ** y))
        | Int x, Int y -> Ok (Int(int_of_float (float_of_int x ** float_of_int y)))
          | _ -> Error (InvalidOperation ("Invalid exponentation between " ^ stringify_lit left ^ " and " ^ stringify_lit right)))

    | Add (l, r) -> 
      let* left = eval_expr envs l in 
      let* right = eval_expr envs r in 
        (match left, right with 
          | Float x, Float y  -> Ok (Float(x +. y))
          | Float x, Int y -> Ok (Float(x +. float_of_int y))
          | Int x, Float y -> Ok (Float(float_of_int x +. y))
          | Int x, Int y -> Ok (Int(int_of_float (float_of_int x +. float_of_int y)))
          | _ -> Error (InvalidOperation ("Cannot add " ^ stringify_lit left ^ " and " ^ stringify_lit right)))

    | Subtract (l, r) -> 
      let* left = eval_expr envs l in 
      let* right = eval_expr envs r in 
        (match left, right with 
          | Float x, Float y  -> Ok (Float(x -. y))
          | Float x, Int y -> Ok (Float(x -. float_of_int y))
          | Int x, Float y -> Ok (Float(float_of_int x -. y))
          | Int x, Int y -> Ok (Int(int_of_float (float_of_int x -. float_of_int y)))
          | _ -> Ok (Float(0.0)))

    | Multiply (l, r) -> 
      let* left = eval_expr envs l in 
      let* right = eval_expr envs r in 
        (match left, right with 
          | Float x, Float y  -> Ok (Float(x *. y))
          | Float x, Int y -> Ok (Float(x *. float_of_int y))
          | Int x, Float y -> Ok (Float(float_of_int x *. y))
          | Int x, Int y -> Ok (Int(int_of_float (float_of_int x *. float_of_int y)))
          | _ -> Ok (Float(0.0)))

    | Neq (l, r) -> 
      let* left = eval_expr envs l in 
      let* right = eval_expr envs r in 
      Ok (Boolean(left != right))

    | Greater (l, r) -> 
      let* left = eval_expr envs l in 
      let* right = eval_expr envs r in 
      Ok (Boolean(left > right))

    | _ -> Ok (Float(0.0))

let rec is_truthy expr envs = match expr with 
  Boolean x -> Ok x
  | Int x -> Ok (x > 0) 
  | Float x -> Ok (x > 0.0)
  | String x -> Ok (String.length x > 0)
  | _ -> 
    let* expr = eval_expr envs expr in is_truthy expr envs

let rec eval_program prog_block = 
  let env = Env.create 16 in 
  let* () = eval_block prog_block [env] in
  let _ = Env.print env 
  in Ok ()

and eval_block block envs =
  let* () = eval_stmts block.stmts envs in
  Ok () 

and eval_stmts stmts envs = match stmts with 
  [] -> Ok () 
  | x :: xs -> 
    let* () = eval_stmt x envs 
      in eval_stmts xs envs
      
and eval_stmt stmt envs: (unit, exn) result = match stmt with 
  | AssignStmt s -> 
    let right = eval_expr envs s.right in (match right with
      Ok r -> let _ = Env.add envs s.ident r s.is_local in Ok ()
    | Error e -> Error e)
  | WhileLoop wl -> eval_while_loop wl envs
  | _ -> Ok ()

and eval_while_loop (wl: wl) sym_table =
  let new_env = Env.create 16 in
  let sym_table = new_env :: sym_table in   
    let rec eval_while_loop_aux (wl: wl) sym_table = 
      let* truthy = is_truthy wl.condition sym_table in 
        if truthy then 
          let* () = eval_block wl.body sym_table in 
            eval_while_loop_aux wl sym_table  
        else 
          Ok ()
    in 
      eval_while_loop_aux wl sym_table