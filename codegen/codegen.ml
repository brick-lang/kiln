(*===----------------------------------------------------------------------===
 * Code Generation
 *===----------------------------------------------------------------------===*)

open Core
open Util
open ParseTree

exception Error of string

let context = Llvm.global_context ();;
let the_module = Llvm.create_module context "brick program";;
let builder = Llvm.builder context;;
let named_values = String.Table.create ~size:10 ();;
let double_type = Llvm.double_type context;;
let integer_type = Llvm.i64_type context;;
let float_type = Llvm.float_type context;;

let handle_proto (name : string) args : Llvm.llvalue =
  (* Make the function type: double(double,double) etc. *)
  let floats = Array.create (List.length args) integer_type in
  let ft = Llvm.function_type integer_type floats in
  let f = match Llvm.lookup_function name the_module with
    | None -> Llvm.declare_function name ft the_module

    (* If 'f' conflicted, there was already something named 'name'. If it
     * has a body, don't allow redefinition or reextern. *)
    | Some f ->
        (* If 'f' already has a body, reject this. *)
        if Llvm.block_begin f <> Llvm.At_end f then
          raise (Error "redefinition of function")

        (* If 'f' took a different number of arguments, reject. *)
        else if Llvm.element_type (Llvm.type_of f) <> ft then
          raise (Error "redefinition of function with different # args")
        else f
  in
  let args = List.map args ~f:(fun {PatternDefault.pattern = p; _} -> p) in
  List.iter2_exn (Array.to_list (Llvm.params f)) args ~f:(fun param var ->
      let name = match var with { Pattern.variant = (Pattern.Ref_variable {Asttypes.txt = s; _}); _} -> s
                              | { Pattern.variant = (Pattern.Variable {Asttypes.txt = s; _}); _} -> s
                              | _ -> assert false
      in
      Llvm.set_value_name name param;
      ignore (String.Table.add named_values name param);
    ); 
  f;;


let rec codegen_expr { Expression.variant = v; _} = let open Expression in match v with
  | Constant c -> codegen_constant c
  (* | Ident { Asttypes.txt = i; _} -> let i = Fqident.last i in Llvm.build_ret () *)
  | _ -> assert false

(* and codegen_pattern { Pattern.variant = v; _} = *)
(*   let open Pattern in *)
(*   match v with *)
(*   | Variable v -> Module.local T.i32 v.Asttypes.txt *)
(*   | _ -> assert false *)

(* and codegen_patterns ps = *)
(*   let open Pattern in *)
(*   (\* TODO: Make this handle more than Variables *\) *)
(*   let ps = List.map ps ~f:(fun { Pattern.variant = v; _} -> *)
(*       match v with *)
(*       | Variable v -> v.Asttypes.txt *)
(*       | _ -> assert false) *)
(*   in *)
(*   let%m vs = Module.locals T.i32 ps in *)
(*   if List.contains_dup ps then *)
(*     failwith "Multiple vars of same name." *)
(*   else if List.length ps <> List.length vs then *)
(*     failwith "Unable to bind all vars properly." *)
(*   else begin *)
(*     (\* let curr_vars_set = String.Set.of_list @@ String.Table.keys env in *\) *)
(*     (\* let new_vars_set = String.Set.of_list ps in *\) *)
(*     (\* let inter = String.Set.inter curr_vars_set new_vars_set in *\) *)
(*     (\* List.iter inter ~f:(fun v -> printf "Warning: Variable %s shadows previous declaration." v); *\) *)
(*     List.iter2_exn ps vs ~f:(fun p v -> String.Table.add_exn env ~key:p ~data:v); *)
(*     return @@ lazy vs *)
(*   end *)

(* and codegen_pattern_defaults (ps:PatternDefault.t list) = *)
(*   let open PatternDefault in *)
(*   let ps = List.map ps ~f:(fun {pattern = p; _} -> p) in *)
(*   codegen_patterns ps *)
and codegen_structure_item { StructureItem.variant = v; _} = let open StructureItem in match v with
  | Value vb -> codegen_value_binding vb
  | _ -> assert false
and codegen_value_binding { ValueBinding.pattern = p; ValueBinding.expression = e; _} = match (p,e) with
  | ({ Pattern.variant    = (Pattern.Variable {Asttypes.txt = name; _}); _},
     { Expression.variant = (Expression.Function (args, body)); _}) ->
      String.Table.clear named_values;
      let the_function = handle_proto name args in

      (* Create a new basic block to start insertion into. *)
      let bb = Llvm.append_block context "entry" the_function in
      Llvm.position_at_end bb builder;
      (try
         let ret_val = codegen_expr body in

         (* Finish off the function. *)
         let _ = Llvm.build_ret ret_val builder in

         (* Validate the generated code, checking for consistency. *)
         (* Llvm_analysis.assert_valid_function the_function; *)

         the_function
       with e ->
         Llvm.delete_function the_function;
         raise e)


  | _ -> assert false
and codegen_constant c = let open Asttypes in match c with 
  | Const_int i -> Llvm.const_int (Llvm.i64_type context) i
  | Const_float f -> Llvm.const_float (Llvm.float_type context) f
  | _ -> assert false

(* | Parsetree.Integer (num) -> Llvm.const_int (Llvm.i64_type context) num *)
(*   | Parsetree.Float (num) -> Llvm.const_float (Llvm.float_type context) num *)
(*   | Parsetree.Bool  (b)   -> Llvm.const_int (Llvm.i1_type context) (match b with true -> 1 | false -> 0) *)
(*   | Parsetree.Call(callee, args) ->  *)
(*       let callee = match Llvm.lookup_function callee the_module with *)
(*         | Some callee -> callee *)
(*         | None -> raise (Error "unknown function reference") *)
(*       in *)
(*       let params = Llvm.params callee in *)
(*       (\* If argument mismatch error. *\) *)
(*       if (Array.length params) <> (List.length args) *)
(*       then raise (Error "incorrect number of arguments passed"); *)
(*       let args = List.map args ~f:(fun e -> codegen_expr e) in *)
(*       Llvm.build_call callee (Array.of_list args) "calltmp" builder *)

(*   | Parsetree.Function (name, args, body, _) -> *)
(*       String.Table.clear named_values; *)
(*       let the_function = handle_proto name args in *)

(*       (\* Create a new basic block to start insertion into. *\) *)
(*       let bb = Llvm.append_block context "entry" the_function in *)
(*       Llvm.position_at_end bb builder; *)
(*       (try *)
(*          let ret_val = codegen_expr body in *)

(*          (\* Finish off the function. *\) *)
(*          let _ = Llvm.build_ret ret_val builder in *)

(*          (\* Validate the generated code, checking for consistency. *\) *)
(*          Llvm_analysis.assert_valid_function the_function; *)

(*          the_function *)
(*        with e -> *)
(*          Llvm.delete_function the_function; *)
(*          raise e) *)

(*   | Parsetree.Block(el) -> codegen_expr (match (List.last el) with Some e -> e | None -> assert false) *)
(*   | Parsetree.Ident(n) -> print_string "Whoops! IDENT"; assert false *)
(*   | _ -> assert false *)

let dump_module () = print_string @@ Llvm.string_of_llmodule the_module
(* Llvm_irreader *)
(* Llvm_all_backends.initialize (); *)
(* let triple = Llvm_target.Target.default_triple () in *)
(* let target = Llvm_target.Target.by_triple triple in *)
(* let machine = Llvm_target.TargetMachine.create triple target in *)
(* let ft = Llvm_target.CodeGenFileType.ObjectFile in *)
(* Llvm_target.TargetMachine.emit_to_file the_module ft "a.out" machine *)
