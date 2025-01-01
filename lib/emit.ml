(* open Ast;;
let emit_expr expr = match expr with 
  Int x -> Printf.sprintf ("int ")
  | _ -> ""


let emit_stmt stmt = match stmt with 
  | AssignStmt s ->
    emit_expr s.ident ^ " = " ^ 
  | _ -> "" *)