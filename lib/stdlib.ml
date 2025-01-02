let print args = 
  let str = List.fold_left (fun acc x -> acc ^ (Ast.stringify_lit x) ^ " ") "" args in 
  let _ = print_endline str 
    in Ast.Nil

let to_string args = Ast.String(Ast.stringify_lit (List.hd args))

let typeof args = 
  let expr = List.hd args in match expr with 
    Ast.Int _ -> Ast.String("integer")
  | Ast.Float _ -> Ast.String("float")
  | Ast.String _ -> Ast.String("string")
  | Ast.Boolean _ -> Ast.String("boolean")
  | _ -> Ast.String("unknown type")

let builtins = Hashtbl.create 16
let () = Hashtbl.add builtins "print" print
let () = Hashtbl.add builtins "type" typeof
let () = Hashtbl.add builtins "tostring" to_string

let builtins_list = [
  ("print", print); ("string", to_string); ("type", typeof)
]
