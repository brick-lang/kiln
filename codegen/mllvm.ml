(* Monadic LLVM bindings *)

open Core

module Env = struct
  type t = {
    m_context : Llvm.llcontext;
    m_module  : Llvm.llmodule option;
    m_builder : Llvm.llbuilder;

    m_bindings : (string, Llvm.llvalue) Hashtbl.t
  }
end

module Types = struct
  open Llvm
  type t = {
    i1  : lltype;
    i8  : lltype;
    i16 : lltype;
    i32 : lltype;
    i64 : lltype;

    float   : lltype;
    double  : lltype;
    x86fp80 : lltype;
    fp128   : lltype;
    ppc_fp128 : lltype;

    void : lltype;
    label : lltype;
    x86_mmx : lltype;
  }

  let create ctxt = {
    i1 = i1_type ctxt;
    i8 = i8_type ctxt;
    i16 = i16_type ctxt;
    i32 = i32_type ctxt;
    i64 = i64_type ctxt;

    float = float_type ctxt;
    double = double_type ctxt;
    x86fp80 = x86fp80_type ctxt;
    fp128 = fp128_type ctxt;
    ppc_fp128 = ppc_fp128_type ctxt;

    void = void_type ctxt;
    label = label_type ctxt;
    x86_mmx = x86_mmx_type ctxt;
  }
end

module State = Util.StateMonad.Make(Env)
include State

(*===-- Modules -----------------------------------------------------------===*)
let create_module name = fun ({Env.m_context = c; _} as env) ->
  let modul = Llvm.create_module c name in
  modul, {env with m_module = Some(modul) }

let dispose_module () = fun ({Env.m_module = m; _} as env) -> match m with
  | None -> (), env
  | Some(m) -> Llvm.dispose_module m, {env with m_module = None}

let target_triple () = fun ({Env.m_module = m; _} as env) -> match m with
  | None -> failwith "Cannot get target triple of non-existent module"
  | Some(m) -> Llvm.target_triple m, env

let set_target_triple triple = fun ({Env.m_module = m; _} as env) -> match m with
  | None -> failwith "Cannot set target triple of non-existent module"
  | Some(m) -> Llvm.set_target_triple triple m, env

let data_layout () = fun ({Env.m_module = m; _} as env) -> match m with
  | None -> failwith "Cannot get data layout of non-existent module"
  | Some(m) -> Llvm.data_layout m, env

let set_data_layout dl = fun ({Env.m_module = m; _} as env) -> match m with
  | None -> failwith "Cannot set data layout of non-existent module"
  | Some(m) -> Llvm.set_data_layout dl m, env

let dump_module () = fun ({Env.m_module = m; _} as env) -> match m with
  | None -> failwith "Cannot dump IR of non-existent module"
  | Some(m) -> Llvm.dump_module m, env

let print_module file = fun ({Env.m_module = m; _} as env) -> match m with
  | None -> failwith "Cannot print IR of non-existent module"
  | Some(m) -> Llvm.print_module file m, env

let string_of_llmodule () = fun ({Env.m_module = m; _} as env) -> match m with
  | None -> failwith "Cannot output string of non-existent module"
  | Some(m) -> Llvm.string_of_llmodule m, env

let set_module_inline_asm asm = fun ({Env.m_module = m; _} as env) -> match m with
  | None -> failwith "Cannot add inline assembly in non-existent module"
  | Some(m) -> Llvm.set_module_inline_asm m asm, env

let module_context = fun ({Env.m_module = m; _} as env) -> match m with
  | None -> failwith "Cannot get context of non-existent module"
  | Some(m) -> Llvm.module_context m, env

(*===-- Types -------------------------------------------------------------===*)

(*--... Operations on integer types ........................................--*)
let integer_type width = fun ({Env.m_context = c; _} as env) -> Llvm.integer_type c width, env





let init () =
  let c = Llvm.global_context () in
  { Env.m_context = c;
    Env.m_module  = None;
    Env.m_builder = Llvm.builder c;
    Env.m_bindings = String.Table.create () ~size:10 }
