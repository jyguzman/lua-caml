open Result;;

exception Incorrect_number_of_arguments of string;;

let print args = 
  let str = List.fold_left (fun acc x -> acc ^ (Ast.stringify_lit x) ^ " ") "" args in 
  let _ = print_endline str 
    in Ok Ast.Nil

let to_string args = 
  match List.length args with 
    1 -> Ok (Ast.String(Ast.stringify_lit (List.hd args)))
    |_ -> Error (Incorrect_number_of_arguments "tostring requires at most one argument")

let typeof args = 
  match List.length args with 
    1 -> let res = match List.hd args with 
        Ast.Int _ -> Ast.String("integer")
      | Ast.Float _ -> Ast.String("float")
      | Ast.String _ -> Ast.String("string")
      | Ast.Boolean _ -> Ast.String("boolean")
      | _ -> Ast.String("unknown type") in Ok (res)
  |_ -> Error (Incorrect_number_of_arguments "typeof requires at most one argument")

let builtins = Hashtbl.create 16
let () = Hashtbl.add builtins "print" print
let () = Hashtbl.add builtins "type" typeof
let () = Hashtbl.add builtins "tostring" to_string

let builtins_list = [
  ("print", print); ("string", to_string); ("type", typeof)
]
