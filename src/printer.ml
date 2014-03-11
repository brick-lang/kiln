open Core.Std

let string_of_type_annot = function
  | None -> ""
  | Some(s) -> Printf.sprintf " : %s" s


let string_of_variable = function
  | Ast.Ref_Var(s, t) -> Printf.sprintf "!%s%s" s (string_of_type_annot t)
  | Ast.Plain_Var(s, t) -> Printf.sprintf "%s%s" s (string_of_type_annot t)


let rec string_of_expr = function
  | Ast.Integer(i,t) -> Printf.sprintf "%i%s" i (string_of_type_annot t)
  | Ast.Float(f,t)   -> Printf.sprintf "%f%s" f (string_of_type_annot t)
(*  | Ast.Binary(op, e1, e2) -> Printf.sprintf "%s %s %s" (string_of_expr e1) op (string_of_expr e2) *)
  | Ast.Bool(b,t)    -> Printf.sprintf "%s%s" (Bool.to_string b) (string_of_type_annot t)
  | Ast.Ident(s) -> s
  | Ast.Block(es, _) -> 
      let bls = (List.map es ~f:(fun x -> string_of_expr x)) in
      let string_of_exprs = List.fold_left bls ~init:"" ~f:(fun x y -> x ^ y ^ "; ") in 
      Printf.sprintf "{ %s}" string_of_exprs
  | Ast.Function(name, var_list, block) -> 
    let exs = (Core.Std.List.map var_list ~f:(fun x -> string_of_variable x)) in
    let rec string_of_vars = Core.Std.List.fold_left exs ~init:"" ~f:(fun x y -> x ^ (if x = "" then "" else ",") ^ y) in
    if name = "" 
    then Printf.sprintf "|%s| -> %s" string_of_vars (string_of_expr block)
    else Printf.sprintf "fn %s(%s) -> %s" name string_of_vars (string_of_expr block)
    

let string_of_call = function
    | Ast.Forked_Call(e) -> string_of_expr e
    | Ast.Pipelined_Call(e) -> string_of_expr e
    | Ast.No_Call -> "()"

let string_of_binding = function
  | Ast.Binding(v,e,c) -> Printf.sprintf "%s = %s -> %s" (string_of_variable v) (string_of_expr e) (string_of_call c)

let string_of_stmt = function
  | Ast.Let(bl, e) -> 
    let bls = (Core.Std.List.map bl ~f:(fun x -> string_of_binding x)) in
    let string_of_bindings = Core.Std.List.fold_left bls ~init:"" ~f:(fun x y -> x ^ " | " ^ y) in
    Printf.sprintf "let [%s] in %s" string_of_bindings (string_of_expr e)
  | Ast.Top(e) -> string_of_expr e
  | _ -> assert false


let rec string_of_file_input = function
  | [] -> ""
  | x::xs -> (string_of_expr x) ^"\n"^(string_of_file_input xs)
