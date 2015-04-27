open Parsetree
(* open Types *)
open Typedtree


(* Typing of constants *)

let type_constant = function
  | Const_int _    -> instance_def Predef.type_int
  | Const_string _ -> instance_def Predef.type_string
  | Const_float _  -> instance_def Predef.type_float
  | Const_int32 _  -> instance_def Predef.type_int32
  | Const_int64 _  -> instance_def Predef.type_int64
