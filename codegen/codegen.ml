(*===----------------------------------------------------------------------===
 * Code Generation
 *===----------------------------------------------------------------------===*)

open Core
open Util
open ParseTree
open Mllvm

let lltypes = Types.create @@ Llvm.global_context ()

(* exception Error of string *)
(* open Ollvm.Ez *)
(* module EzModule = struct *)
(*   include Ollvm.Ez.Module *)
(*   (\* Flip for monads *\) *)
(*   let set_data_layout = flip set_data_layout *)
(*   let set_target_triple = flip3 set_target_triple *)
(*   let local = flip2 local *)
(*   let locals = flip2 locals *)
(*   let batch_locals = flip batch_locals *)
(*   let global = flip2 global *)
(*   let declaration = flip declaration *)
(*   let definition = flip definition *)
(*   let lookup_declaration_exn = flip lookup_declaration *)
(*   let lookup_declaration s e = try Some (lookup_declaration e s) with Not_found -> None *)
(*   let lookup_definition_exn = flip lookup_definition *)
(*   let lookup_definition s e = try Some (lookup_definition e s) with Not_found -> None *)
(* end *)
(* module ModuleMonad = StateMonad.Make(EzModule) *)
(* module T = Ollvm.Ez.Type *)
(* open ModuleMonad *)
(* open ParseTree *)

(* let env = String.Table.create () *)

(* let rec codegen_expr { Expression.variant = v; _} = *)
(*   let open Expression in *)
(*   match v with *)
(*   | Constant c -> return @@ lazy (Instr.ret @@ codegen_constant c) *)
(*   | Ident { Asttypes.txt = i; _} -> *)
(*       (\* TODO: Extend qualifications *\) *)
(*       let i = Fqident.last i in *)
(*       return @@ lazy (Instr.ret @@ String.Table.find_exn env i) *)
(*   | _ -> assert false *)

(* and codegen_pattern { Pattern.variant = v; _} = *)
(*   let open Pattern in *)
(*   match v with *)
(*   | Variable v -> EzModule.local T.i32 v.Asttypes.txt *)
(*   | _ -> assert false *)

(* and codegen_patterns ps = *)
(*   let open Pattern in *)
(*   (\* TODO: Make this handle more than Variables *\) *)
(*   let ps = List.map ps ~f:(fun { Pattern.variant = v; _} -> *)
(*       match v with *)
(*       | Variable v -> v.Asttypes.txt *)
(*       | _ -> assert false) *)
(*   in *)
(*   let%m vs = EzModule.locals T.i32 ps in *)
(*   if List.contains_dup ps then *)
(*     failwith "Multiple vars of same name." *)
(*   else if List.length ps <> List.length vs then *)
(*     failwith "Unable to bind all vars properly." *)
(*   else begin *)
(*       (\* let curr_vars_set = String.Set.of_list @@ String.Table.keys env in *\) *)
(*       (\* let new_vars_set = String.Set.of_list ps in *\) *)
(*       (\* let inter = String.Set.inter curr_vars_set new_vars_set in *\) *)
(*       (\* List.iter inter ~f:(fun v -> printf "Warning: Variable %s shadows previous declaration." v); *\) *)
(*     List.iter2_exn ps vs ~f:(fun p v -> String.Table.add_exn env ~key:p ~data:v); *)
(*     return @@ lazy vs *)
(*   end *)

(* and codegen_pattern_defaults (ps:PatternDefault.t list) = *)
(*   let open PatternDefault in *)
(*   let ps = List.map ps ~f:(fun {pattern = p; _} -> p) in *)
(*   codegen_patterns ps *)

(* and codegen_structure_item { StructureItem.variant = v; _} = *)
(*   let open StructureItem in *)
(*   match v with *)
(*   | Value vb -> do_; *)
(*       codegen_value_binding vb; *)
(*       m <-- get; *)
(*       return (lazy m) *)
(*   | _ -> assert false *)

(* and codegen_value_binding { ValueBinding.pattern = p; *)
(*                             ValueBinding.expression = e; _} = *)
(*   match (p,e) with *)
(*   | ({Pattern.variant = (Pattern.Variable {Asttypes.txt = v; _}); _}, *)
(*      {Expression.variant = (Expression.Function (args, fe)); _}) -> *)
(*       let%m m = get in begin *)
(*       match EzModule.lookup_declaration v m with *)
(*         | None -> begin %do *)
(*             f_name  <-- EzModule.global T.i32 v; *)
(*             f_entry <-- EzModule.local T.i32 "entry"; *)
(*             f_args  <-- codegen_pattern_defaults args; *)
(*             f_val   <-- codegen_expr fe; *)
(*             m       <-- get; *)
(*             modify @@ EzModule.definition begin *)
(*                 Block.define f_name f_args [ *)
(*                   Block.block f_entry [ f_val ] *)
(*                 ] *)
(*               end; *)
(*           end *)
(*         | Some _ -> assert false *)
(*       end *)
(*   | _ -> assert false *)

(* and codegen_constant c = *)
(*     let open Asttypes in *)
(*     match c with *)
(*     | Const_int i -> Value.i32 i *)
(*     | Const_float f -> Value.float f *)
(*     | _ -> assert false *)
