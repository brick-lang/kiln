(*===----------------------------------------------------------------------===
 * Code Generation
 *===----------------------------------------------------------------------===*)

open Core.Std
open Util
open ParseTree
  
exception Error of string
  
(* let context = Llvm.global_context ();;
   let the_module = Llvm.create_module context "my cool jit";;
   let builder = Llvm.builder context;;
   let named_values = String.Table.create ~size:10 ();;
   let double_type = Llvm.double_type context;;
   let integer_type = Llvm.i64_type context;;
   let float_type = Llvm.float_type context;;
*)

(* let handle_proto (name : string) (args : variable list) : Llvm.llvalue = 
   (* Make the function type: double(double,double) etc. *)
    let floats = Array.create (List.length args) float_type in
    let ft = Llvm.function_type float_type floats in
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
    List.iter2_exn (Array.to_list (Llvm.params f)) args ~f:(fun param var ->
        let name = match var with Ref_Var(s,_) | Plain_Var(s,_) -> s in
        Llvm.set_value_name name param;
        ignore (String.Table.add named_values name param);
      ); 
    f;;
    
    
   let rec codegen_expr = function
   | Parsetree.Integer (num) -> Llvm.const_int (Llvm.i64_type context) num
   | Parsetree.Float (num)   -> Llvm.const_float (Llvm.float_type context) num
   | Parsetree.Bool (b)       -> 
      Llvm.const_int (Llvm.i1_type context) (match b with true -> 1 | false -> 0)
   | Parsetree.Call(callee, args) -> 
    let callee = match Llvm.lookup_function callee the_module with
      | Some callee -> callee
      | None -> raise (Error "unknown function reference")
    in
    let params = Llvm.params callee in
    (* If argument mismatch error. *)
    if (Array.length params) <> (List.length args)
    then raise (Error "incorrect number of arguments passed");
    let args = List.map args ~f:(fun e -> codegen_expr e) in
    Llvm.build_call callee (Array.of_list args) "calltmp" builder

   | Parsetree.Function (name, args, body, _) ->
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
      Llvm_analysis.assert_valid_function the_function;

      the_function
    with e ->
      Llvm.delete_function the_function;
      raise e)
      
   | Parsetree.Block(el) -> codegen_expr (match (List.last el) with Some e -> e | None -> assert false)
   | Parsetree.Ident(n) -> print_string "Whoops! IDENT"; assert false
   | _ -> assert false
*)

module M = struct
  include Ollvm.Ez.Module
  (* Flip for monads *)
  let set_data_layout = flip set_data_layout
  let set_target_triple = flip3 set_target_triple
  let local = flip2 local
  let locals = flip2 locals
  let batch_locals = flip batch_locals
  let global = flip2 global
  let declaration = flip declaration
  let definition = flip definition
  let lookup_declaration = flip lookup_declaration
  let lookup_definition = flip lookup_definition
end
module T = Ollvm.Ez.Type
module Printer = Ollvm.Printer
module ModuleMonad = StateMonad.Make(M)
				    
open Ollvm.Ez.Value
open Ollvm.Ez.Instr
open Ollvm.Ez.Block
open ModuleMonad
  
let codegen_expr { ParseTree.Expression.variant = v; _}  =
  let open ParseTree.Nodes.Expression in match v with
  | Constant c -> begin
      let open Asttypes in
      match c with
      | Const_int i -> (i32 i)
      | Const_int32 i -> (i32 i)
      | Const_int64 i -> (i64 i)
    end
  | _ -> assert false
  

