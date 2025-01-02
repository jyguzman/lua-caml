type builtin = 
  | Function of {
    args: Ast.expr list;
    return: Ast.expr;

  }

let print args = 
  let str = List.fold_left (fun acc x -> acc ^ (Ast.stringify_lit x) ^ " ") "" args in 
  let _ = print_endline str 
    in Ast.Nil

let to_string args = Ast.String(Ast.stringify_lit (List.hd args))

let builtins = Hashtbl.create 16
let () = Hashtbl.add builtins "print" print

let builtins_list = [
  ("print", print); ("string", to_string);
]
