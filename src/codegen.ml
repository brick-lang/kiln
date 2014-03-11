(*===----------------------------------------------------------------------===
 * Code Generation
 *===----------------------------------------------------------------------===*)

open Core.Std
open Ast

exception Error of string

let context = Llvm.global_context ();;
let the_module = Llvm.create_module context "my cool jit";;
let builder = Llvm.builder context;;
let named_values = String.Table.create ~size:10 ();;
let double_type = Llvm.double_type context;;
let integer_type = Llvm.i64_type context;;
let float_type = Llvm.float_type context;;


let rec codegen_expr = function
  | Ast.Integer (num, _) -> Llvm.const_int (Llvm.i64_type context) num
  | Ast.Float (num, _)   -> Llvm.const_float (Llvm.float_type context) num
  | Ast.Bool (b,_)       -> 
      Llvm.const_int (Llvm.i1_type context) (match b with true -> 1 | false -> 0)
  | Ast.Call(callee, args) -> 
    let callee = match Llvm.lookup_function callee the_module with
      | Some callee -> callee
      | None -> raise (Error "unknown function reference")
    in 
    let params = params callee in 
    
    (* If argument mismatch error. *)
    if Array.length params <> Array.length args
    then raise (Error "incorrect number of arguments passed")
    else ();
    let args = List.map args ~f:codegen_expr in
    Llvm.build_call callee args "calltmp" builder

  | Ast.Function (name, args, expr) -> 
    (* FIXME: this should be fixed *)
    let doubles 
 







