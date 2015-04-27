(* let get_type_of_ *)
open Core.Std

module TypeVariable = struct
  type t = { id:int; mutable name:char option; mutable instance:string option }
  let next_variable_id = ref 0
  let next_variable_name = ref 'A'
  let get_next_variable_id () =
    let id = !next_variable_id in
    incr next_variable_id;
    id

  let get_next_variable_name () = 
    let name = !next_variable_name in
    (next_variable_name := 
       match  Char.of_int ((Char.to_int !next_variable_name) + 1) with 
       | Some x -> if Char.is_uppercase x then x else 'A'
       | None -> assert false);
    name

  let get_name v = 
    match v.name with
    | None -> let name = get_next_variable_name () in v.name <- Some(name); name
    | Some(n) -> n

  let to_string v =
    match v.instance with
    | None -> String.of_char (get_name v)
    | Some(i) -> i

  let create () = { id = get_next_variable_id (); name = None; instance = None }
end



let rec get_type_of_expr_from_env (expr : Ast.expr) env : Ast.type_annot = 
  match expr with
  | Ast.Bool(_, t)
  | Ast.Integer(_, t)
  | Ast.Float(_,t) 
  | Ast.Block(_,t) 
  | Ast.Function(_,_,_,t)
  | Ast.Call(_,_,t) -> t
  | Ast.Ident(n,t) ->
      (* TODO: try to unify ident type and calculated type here *)
      String.Map.find env n


(* let get_type_of_ident (id : string) env ?non_generic = *)
(*   let non_generic = match non_generic with None -> Set.empty | Some x -> x in *)
(*   |  *)

and analyse_expr ?(non_generic = Set.Poly.empty) ?(env = String.Map.empty) (node : Ast.expr)  =
  match node with
  | Ast.Bool(_,_)
  | Ast.Integer(_,_)
  | Ast.Float(_,_) -> get_type_of_expr_from_env node env
  | Ast.Function(n,vl,b,t) -> 
      let arg_types = List.map vl ~f:(fun e -> TypeVariable.create ()) in 
      let new_env = 
        let tmp_map = ref env in
        (* TODO: Unify variable types with typevariable list here *)
        List.iter2_exn vl arg_types ~f:(fun v t -> 
            match v with Ast.Ref_Var(n,_) | Ast.Plain_Var(n,_) -> 
              tmp_map := Map.add !tmp_map ~key:n ~data:t);
        !tmp_map
      in
      let new_non_generic = 
        let tmp_set = ref non_generic in
        List.iter arg_types ~f:(fun t -> tmp_set := Set.add !tmp_set t);
        !tmp_set
      in
      let result_type = analyse_expr b ~non_generic:new_non_generic ~env:new_env in
      Ast.Function(n,vl,b,t)


let analyse_top_stmt ?(non_generic = Set.Poly.empty) (node : Ast.stmt)  =
  match node with
  | Ast.Top e -> analyse_expr e ~non_generic:(non_generic)
  | _ -> assert false


let typing_pass n = analyse_top_stmt n
