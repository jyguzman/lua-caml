open Ast;;
open Env;; 
exception InvalidOperation of string;;

let ( let* ) r f = match r with 
  Ok v -> f v 
| Error e -> Error e

let raise_invalid_op msg = raise (InvalidOperation msg)

let rec eval_expr envs expr = 
  match expr with
    | Int _ | Float _ | String _ | Boolean _ | Nil | Function _ -> Ok expr
    | Name x -> Env.resolve envs x 
    | Grouping x -> eval_expr envs x

    | Concat (l, r) -> 
      let* left = eval_expr envs l in 
      let* right = eval_expr envs r in 
        (match left, right with 
          | String x, String y -> Ok (String(String.cat x y))
          | String x, _ -> Error (InvalidOperation ("Cannot concatenate string \"" ^ x ^ "\" and " ^ stringify_lit right))
          | _, String y -> Error (InvalidOperation ("Cannot concatenate string \"" ^ y ^ "\" and " ^ stringify_lit left))
          | _ -> Error (InvalidOperation ("Can only concatenate strings, not " ^ stringify_lit left ^ " and " ^ stringify_lit right))) 
    
    | Negate x -> 
      let* y = eval_expr envs x in 
        (match y with 
            Int _ -> eval_expr envs (Multiply(Int(-1), y))
          | Float _ -> eval_expr envs (Multiply(Float(-1.0), y))
          | _ -> Error (InvalidOperation ("Cannot negate" ^ stringify_lit x)))

    | Not x -> 
      let* y = eval_expr envs x in (match y with 
        Boolean x -> Ok (Boolean(not x))
      | _ -> Error (InvalidOperation ("Cannot take the boolean not of" ^ stringify_lit x)))

    | And (l, r) -> 
      let* left = eval_expr envs l in 
      let* right = eval_expr envs r in 
        let* is_truthy_left = is_truthy left envs in 
        let* is_truthy_right = is_truthy right envs in
          Ok (Boolean(is_truthy_left && is_truthy_right))

    | Or (l, r) -> 
      let* left = eval_expr envs l in 
      let* right = eval_expr envs r in 
        let* is_truthy_left = is_truthy left envs in 
        let* is_truthy_right = is_truthy right envs in
          Ok (Boolean(is_truthy_left || is_truthy_right))

    | Power (l, r) -> 
      let* left = eval_expr envs l in 
      let* right = eval_expr envs r in 
        (match left, right with 
        | Float x, Float y  -> Ok (Float(x ** y))
        | Float x, Int y -> Ok (Float(x ** float_of_int y))
        | Int x, Float y -> Ok (Float(float_of_int x ** y))
        | Int x, Int y -> Ok (Float(float_of_int x ** float_of_int y))
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
          | _ -> Error (InvalidOperation ("Cannot subtract " ^ stringify_lit left ^ " and " ^ stringify_lit right)))

    | Multiply (l, r) -> 
      let* left = eval_expr envs l in 
      let* right = eval_expr envs r in 
        (match left, right with 
          | Float x, Float y  -> Ok (Float(x *. y))
          | Float x, Int y -> Ok (Float(x *. float_of_int y))
          | Int x, Float y -> Ok (Float(float_of_int x *. y))
          | Int x, Int y -> Ok (Int(int_of_float (float_of_int x *. float_of_int y)))
          | _ -> Error (InvalidOperation ("Cannot multiply " ^ stringify_lit left ^ " and " ^ stringify_lit right)))

    | Divide (l, r) -> 
      let* left = eval_expr envs l in 
      let* right = eval_expr envs r in 
        (match left, right with 
          | Float x, Float y  -> Ok (Float(x /. y))
          | Float x, Int y -> Ok (Float(x /. float_of_int y))
          | Int x, Float y -> Ok (Float(float_of_int x /. y))
          | Int x, Int y -> Ok (Float(float_of_int x *. float_of_int y))
          | _ -> Error (InvalidOperation ("Cannot multiply " ^ stringify_lit left ^ " and " ^ stringify_lit right)))

    | Equal (l, r) -> let* left = eval_expr envs l in let* right = eval_expr envs r in 
      Ok (Boolean(left = right))

    | Neq (l, r) -> 
      let* left = eval_expr envs l in 
      let* right = eval_expr envs r in 
        Ok (Boolean(left != right))

    | Greater (l, r) -> 
      let* left = eval_expr envs l in 
      let* right = eval_expr envs r in 
        Ok (Boolean(left > right))

    | Geq (l, r) -> 
      let* left = eval_expr envs l in 
      let* right = eval_expr envs r in 
        Ok (Boolean(left >= right))

    | Less (l, r) -> 
      let* left = eval_expr envs l in 
      let* right = eval_expr envs r in 
        Ok (Boolean(left < right))
    
    | Leq (l, r) -> 
      let* left = eval_expr envs l in 
      let* right = eval_expr envs r in 
        Ok (Boolean(left <= right))

    | _ -> Ok (Float(0.0))


and is_truthy expr envs = match expr with 
    Boolean x -> Ok x
  | Int x -> Ok (x > 0) 
  | Float x -> Ok (x > 0.0)
  | String x -> Ok (String.length x > 0)
  | Nil -> Ok false
  | _ -> let* expr = eval_expr envs expr in is_truthy expr envs

let rec eval_program prog_block = 
  let env = Env.create 16 in 
    let* () = eval_block prog_block [env] in
      Ok ()

and eval_block block envs =
  let* () = eval_stmts block.stmts envs in
    let _ = List.iter Env.print envs in
      Ok () 

and eval_stmts stmts envs = match stmts with 
  [] -> Ok () 
  | x :: xs -> 
    let* () = eval_stmt x envs 
      in eval_stmts xs envs
      
and eval_stmt stmt envs = match stmt with 
  | AssignStmt s -> 
    let* right = eval_expr envs s.right in
      let _ = Env.add envs s.ident right s.is_local 
        in Ok ()
  | WhileLoop wl -> eval_while_loop wl envs
  | _ -> Ok ()

and eval_while_loop wl sym_table =
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