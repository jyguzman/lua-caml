open Ast;;
open Result;; 

exception NameError of string;;

let ( let* ) r f = match r with 
  Ok v -> f v 
| Error e -> Error e

module Env = struct 
  type t = (string, expr) Hashtbl.t

  let print (t: t) = 
    let stringify_kv_pair key value init = 
      init ^ key ^ ": " ^ Ast.stringify_expr value ^ "\n"
    in 
    let str = Hashtbl.fold stringify_kv_pair t "" in print_string str

  let create size: t = Hashtbl.create size

  let get_opt (t: t) name = Hashtbl.find_opt t name

  let get (t: t) name = Hashtbl.find t name

  let rec resolve (envs: t list) name = 
    match envs with 
    [] -> Error (NameError ("name '" ^ name ^ "' is not defined")) 
    | x :: xs -> 
      let value = get_opt x name in match value with 
        Some v -> Ok v 
      | None -> resolve xs name

  let rec resolve_opt (envs: t list) name = match envs with 
    [] -> None 
    | x :: xs -> 
      let value = get_opt x name in match value with 
        Some v -> Some v 
      | None -> resolve_opt xs name

  let add_local (envs: t list) name expr = 
    Hashtbl.replace (List.hd envs) name expr

  (* Set an already existing symbol *)
  let rec set (envs: t list) name expr = match envs with 
    [] -> () 
    | x :: xs -> 
      let val_opt = Hashtbl.find_opt x name in match val_opt with  
        Some _ -> let _ = Hashtbl.replace x name expr in ()
      | None -> set xs name expr
  
  (* If this symbol is marked local, add it to the current scope. 
    If it's not marked local, check if it exists in any of the scopes.
    If not, add it to the current scope. If it does, set it to the new value
  in whichever scope it's first seen. *)
  let add (envs: t list) name expr is_local =
    if is_local then add_local envs name expr 
    else
      let value = resolve_opt envs name in match value with 
        None -> add_local envs name expr 
      | Some _ -> set envs name expr
      
end
