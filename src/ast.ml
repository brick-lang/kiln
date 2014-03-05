open Core

type type_annot = string option

type variable = 
    | Ref_Var of string * type_annot
    | Plain_Var of string * type_annot

(* The function prototype (declaration) *)
type proto = Prototype of variable list

type expr = 
  | Integer of int
  | Float   of float
  | Binary  of string * expr * expr
  | Bool    of bool
  | Ident   of string
  | Block   of expr list
  | Function of string * variable list * expr

type call =
  | Forked_Call of expr
  | Pipelined_Call of expr
  | No_Call

type binding = Binding of variable * expr * call (* var = expr *)

type stmt =
  | Top        of expr
  | Let        of binding list * expr (* binding list, body *)
  | Sep 
  | End



let string_of_type_annot ta =
  match ta with
  | None -> ""
  | Some(s) -> Printf.sprintf " : %s" s


let string_of_variable v =
  match v with
  | Ref_Var(s, t) -> Printf.sprintf "!%s%s" s (string_of_type_annot t)
  | Plain_Var(s, t) -> Printf.sprintf "%s%s" s (string_of_type_annot t)


let rec string_of_expr e =
  match e with
  | Integer(i) -> string_of_int i
  | Float(f)   -> Float.to_string f
  | Binary(op, e1, e2) -> Printf.sprintf "%s %s %s" (string_of_expr e1) op (string_of_expr e2)
  | Bool(b) -> string_of_bool b
  | Ident(s) -> s
  | Block(es) -> 
      let bls = (Core.Std.List.map es ~f:(fun x -> string_of_expr x)) in
      let string_of_exprs = Core.Std.List.fold_left bls ~init:"" ~f:(fun x y -> x ^ y ^ "; ") in 
      Printf.sprintf "{ %s}" string_of_exprs
  | Function(name, var_list, block) -> 
    let exs = (Core.Std.List.map var_list ~f:(fun x -> string_of_variable x)) in
    let rec string_of_vars = Core.Std.List.fold_left exs ~init:"" ~f:(fun x y -> x ^ (if x = "" then "" else ",") ^ y) in
    if name = "" 
    then Printf.sprintf "|%s| -> %s" string_of_vars (string_of_expr block)
    else Printf.sprintf "fn %s(%s) -> %s" name string_of_vars (string_of_expr block)
    

let string_of_call c =
    match c with 
    | Forked_Call(e) -> string_of_expr e
    | Pipelined_Call(e) -> string_of_expr e
    | No_Call -> "()"

let string_of_binding b = 
  match b with 
  | Binding(v,e,c) -> Printf.sprintf "%s = %s -> %s" (string_of_variable v) (string_of_expr e) (string_of_call c)

let string_of_stmt s = 
  match s with
  | Let(bl, e) -> 
    let bls = (Core.Std.List.map bl ~f:(fun x -> string_of_binding x)) in
    let string_of_bindings = Core.Std.List.fold_left bls ~init:"" ~f:(fun x y -> x ^ " | " ^ y) in
    Printf.sprintf "let [%s] in %s" string_of_bindings (string_of_expr e)
  | Top(e) -> string_of_expr e
  | _ -> assert false


let rec string_of_file_input top =
  match top with
  | [] -> ""
  | x::xs -> (string_of_expr x) ^"\n"^(string_of_file_input xs)
