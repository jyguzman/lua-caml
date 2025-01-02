open Ast;;

let print args = 
  let rec print_aux args =
    match args with 
      [] -> ()
      | x :: xs ->
        let _ = (match x with 
          Int x -> print_string (string_of_int x ^ "\n")
        | Float x -> print_string (string_of_float x ^ "\n")
        | String x -> print_string (x ^ "\n")
        | Boolean x -> print_string (if x then "true" else "false")
        | Nil -> print_string "nil"
        | _ -> ())
      in print_aux xs
in
  print_aux args


let builtins = Hashtbl.create 16
let () = Hashtbl.add builtins "print" print
