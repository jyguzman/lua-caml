open Ast;;
open Env;; 
open Builtins;;
exception Invalid_operation of string;;

let ( let* ) r f = match r with 
  Ok v -> f v 
| Error e -> Error e

let ( let+ ) o f = match o with 
  Some v -> f v 
| None -> None

let raise_invalid_op msg = raise (Invalid_operation msg)

let rec eval_expr sym_tbl expr = 
  match expr with
    | Int _ | Float _ | String _ | Boolean _ | Nil | Function _ -> Ok expr
    | Name x -> Env.resolve sym_tbl x
    | Grouping x -> eval_expr sym_tbl x
    (* | FunctionCall fc -> eval_fun_call_expr sym_tbl fc *)

    | Concat (l, r) -> 
      let* left = eval_expr sym_tbl l in 
      let* right = eval_expr sym_tbl r in 
        (match left, right with 
          | String x, String y -> Ok (String(String.cat x y))
          | String x, _ -> Error (Invalid_operation ("Cannot concatenate string \"" ^ x ^ "\" and " ^ stringify_lit right))
          | _, String y -> Error (Invalid_operation ("Cannot concatenate string \"" ^ y ^ "\" and " ^ stringify_lit left))
          | _ -> Error (Invalid_operation ("Can only concatenate strings, not " ^ stringify_lit left ^ " and " ^ stringify_lit right))) 
    
    | Negate x -> 
      let* y = eval_expr sym_tbl x in 
        (match y with 
            Int _ -> eval_expr sym_tbl (Multiply(Int(-1), y))
          | Float _ -> eval_expr sym_tbl (Multiply(Float(-1.0), y))
          | _ -> Error (Invalid_operation ("Cannot negate" ^ stringify_lit x)))

    | Not x -> 
      let* y = eval_expr sym_tbl x in (match y with 
        Boolean x -> Ok (Boolean(not x))
      | _ -> Error (Invalid_operation ("Cannot take the boolean not of" ^ stringify_lit x)))

    | And (l, r) -> 
      let* left = eval_expr sym_tbl l in 
      let* right = eval_expr sym_tbl r in 
        let* is_truthy_left = is_truthy left sym_tbl in 
        let* is_truthy_right = is_truthy right sym_tbl in
          Ok (Boolean(is_truthy_left && is_truthy_right))

    | Or (l, r) -> 
      let* left = eval_expr sym_tbl l in 
      let* right = eval_expr sym_tbl r in 
        let* is_truthy_left = is_truthy left sym_tbl in 
        let* is_truthy_right = is_truthy right sym_tbl in
          Ok (Boolean(is_truthy_left || is_truthy_right))

    | Power (l, r) -> 
      let* left = eval_expr sym_tbl l in 
      let* right = eval_expr sym_tbl r in 
        (match left, right with 
        | Float x, Float y  -> Ok (Float(x ** y))
        | Float x, Int y -> Ok (Float(x ** float_of_int y))
        | Int x, Float y -> Ok (Float(float_of_int x ** y))
        | Int x, Int y -> Ok (Float(float_of_int x ** float_of_int y))
          | _ -> Error (Invalid_operation ("Invalid exponentation between " ^ stringify_lit left ^ " and " ^ stringify_lit right)))

    | Add (l, r) -> 
      let* left = eval_expr sym_tbl l in 
      let* right = eval_expr sym_tbl r in 
        (match left, right with 
          | Float x, Float y  -> Ok (Float(x +. y))
          | Float x, Int y -> Ok (Float(x +. float_of_int y))
          | Int x, Float y -> Ok (Float(float_of_int x +. y))
          | Int x, Int y -> Ok (Int(int_of_float (float_of_int x +. float_of_int y)))
          | _ -> Error (Invalid_operation ("Cannot add " ^ stringify_lit left ^ " and " ^ stringify_lit right)))

    | Subtract (l, r) -> 
      let* left = eval_expr sym_tbl l in 
      let* right = eval_expr sym_tbl r in 
        (match left, right with 
          | Float x, Float y  -> Ok (Float(x -. y))
          | Float x, Int y -> Ok (Float(x -. float_of_int y))
          | Int x, Float y -> Ok (Float(float_of_int x -. y))
          | Int x, Int y -> Ok (Int(int_of_float (float_of_int x -. float_of_int y)))
          | _ -> Error (Invalid_operation ("Cannot subtract " ^ stringify_lit left ^ " and " ^ stringify_lit right)))

    | Multiply (l, r) -> 
      let* left = eval_expr sym_tbl l in 
      let* right = eval_expr sym_tbl r in 
        (match left, right with 
          | Float x, Float y  -> Ok (Float(x *. y))
          | Float x, Int y -> Ok (Float(x *. float_of_int y))
          | Int x, Float y -> Ok (Float(float_of_int x *. y))
          | Int x, Int y -> Ok (Int(int_of_float (float_of_int x *. float_of_int y)))
          | _ -> Error (Invalid_operation ("Cannot multiply " ^ stringify_lit left ^ " and " ^ stringify_lit right)))

    | Divide (l, r) -> 
      let* left = eval_expr sym_tbl l in 
      let* right = eval_expr sym_tbl r in 
        (match left, right with 
          | Float x, Float y  -> Ok (Float(x /. y))
          | Float x, Int y -> Ok (Float(x /. float_of_int y))
          | Int x, Float y -> Ok (Float(float_of_int x /. y))
          | Int x, Int y -> Ok (Float(float_of_int x *. float_of_int y))
          | _ -> Error (Invalid_operation ("Cannot multiply " ^ stringify_lit left ^ " and " ^ stringify_lit right)))

    | Equal (l, r) -> let* left = eval_expr sym_tbl l in let* right = eval_expr sym_tbl r in 
      Ok (Boolean(left = right))

    | Neq (l, r) -> 
      let* left = eval_expr sym_tbl l in 
      let* right = eval_expr sym_tbl r in 
        Ok (Boolean(left != right))

    | Greater (l, r) -> 
      let* left = eval_expr sym_tbl l in 
      let* right = eval_expr sym_tbl r in 
        Ok (Boolean(left > right))

    | Geq (l, r) -> 
      let* left = eval_expr sym_tbl l in 
      let* right = eval_expr sym_tbl r in 
        Ok (Boolean(left >= right))

    | Less (l, r) -> 
      let* left = eval_expr sym_tbl l in 
      let* right = eval_expr sym_tbl r in 
        Ok (Boolean(left < right))
    
    | Leq (l, r) -> 
      let* left = eval_expr sym_tbl l in 
      let* right = eval_expr sym_tbl r in 
        Ok (Boolean(left <= right))

    | _ -> Ok (Float(0.0))

and eval_builtin_func sym_tbl name args =
  let func = Hashtbl.find builtins name in
  let* args = eval_expr_list sym_tbl args in 
    func args

and is_truthy expr sym_tbl = match expr with 
    Boolean x -> Ok x
  | Int x -> Ok (x > 0) 
  | Float x -> Ok (x > 0.0)
  | String x -> Ok (String.length x > 0)
  | Nil -> Ok false
  | _ -> let* expr = eval_expr sym_tbl expr in is_truthy expr sym_tbl

and eval_expr_list sym_tbl exprs = 
  let rec eval_expr_list_aux sym_tbl exprs evals = 
    match exprs with 
      [] -> Ok (List.rev evals) 
    | x :: xs -> 
      let* expr = eval_expr sym_tbl x in 
        eval_expr_list_aux sym_tbl xs (expr :: evals)
  in 
    eval_expr_list_aux sym_tbl exprs []

let rec eval_program prog_block = 
  let env = Env.create 16 in 
    let* () = eval_block prog_block [env] in
      Ok ()

and eval_block block sym_tbl = 
  let* () = 
    eval_stmts block.stmts sym_tbl in
      Ok ()

and eval_stmts stmts sym_tbl = match stmts with 
  [] -> Ok () 
  | x :: xs -> 
    match x with 
      ReturnStmt _ -> Ok ()
    | _ -> 
      let* () = eval_stmt x sym_tbl 
        in eval_stmts xs sym_tbl
      
and eval_stmt stmt sym_tbl = match stmt with 
  | AssignStmt s -> eval_assign_stmt s sym_tbl 
  | IfStmt if_stmt -> eval_if_stmt if_stmt sym_tbl
  | WhileLoop wl -> eval_while_loop wl sym_tbl
  | FunctionCall fc -> eval_fun_call_stmt sym_tbl fc
  | Ast.ReturnStmt _ -> Ok ()
  | _ -> Error (Invalid_operation "unrecognized statement type")

and eval_assign_stmt stmt sym_tbl = 
  let* right = eval_expr sym_tbl stmt.right in
  let _ = Env.add sym_tbl stmt.name right stmt.is_local 
    in Ok ()

and eval_while_loop wl sym_table =
  let new_env = Env.create 16 in
  let sym_table = new_env :: sym_table in   
    let rec eval_while_loop_aux wl sym_table = 
      let* truthy = is_truthy wl.while_condition sym_table in 
        if truthy then 
          let* () = eval_block wl.body sym_table in 
            eval_while_loop_aux wl sym_table  
        else 
          Ok ()
  in 
    eval_while_loop_aux wl sym_table

and eval_if_stmt if_stmt sym_tbl =
  let new_env = Env.create 16 in
  let sym_tbl = new_env :: sym_tbl in   
  let* truthy = is_truthy if_stmt.then_condition sym_tbl in
  if truthy then 
    eval_block if_stmt.then_block sym_tbl 
  else 
    match if_stmt.elseif with 
      Some ef -> eval_if_stmt ef sym_tbl 
    | None ->
      match if_stmt.else_block with 
        None -> Ok ()
      | Some block -> eval_block block sym_tbl

and eval_fun_call_stmt sym_tbl fc = 
  match fc.target with 
    | Name x -> 
      let builtin = Hashtbl.find_opt builtins x in 
      let other = Env.resolve_opt sym_tbl x in 
      if builtin <> None then 
        let result = eval_builtin_func sym_tbl x fc.args in 
          (match result with 
            Ok _ -> Ok ()
          | Error e -> Error e)
      else if other <> None then 
        let* func = Env.resolve sym_tbl x in (match func with 
            Ast.Function f -> eval_user_fun_call_stmt f x fc.args sym_tbl
          | _ -> Error (Invalid_operation ("function '" ^ x ^ "' is not defined")))
      else 
        Error (Invalid_operation ("function '" ^ x ^ "' not defined"))
    | Grouping _ | FunctionCall _ -> Error (Invalid_operation "Non-identifier function call not supported yet")

and eval_user_fun_call_stmt func func_name func_args sym_tbl = 
  let* params, args = prepare_func_args func func_name func_args sym_tbl in
  let new_env = Env.create 16 in 
  let new_sym_tbl = (new_env :: sym_tbl) in 
  let _ = List.map2 (fun x y -> Env.add_local new_sym_tbl x y) params args in 
  eval_block func.body.block new_sym_tbl

and prepare_func_args func func_name func_args sym_tbl = 
  let params = func.body.params in 
  let* args = eval_expr_list sym_tbl func_args in 
  let num_args = List.length args in 
  let num_params = List.length params in
  if num_args <> num_params then
    Error (Incorrect_number_of_arguments ("function '" ^ func_name ^ "' has " ^ string_of_int num_params ^ " parameters but got " ^ string_of_int num_args))
  else
    Ok (params, args)

