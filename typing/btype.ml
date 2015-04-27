(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(* Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt*)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Basic operations on core types *)

open Misc
open Types

(**** Sets, maps and hashtables of types ****)

module TypeSet = Set.Make(TypeOps)
module TypeMap = Map.Make (TypeOps)
module TypeHash = Hashtbl.Make(TypeOps)

(**** Forward declarations ****)

let print_raw =
  ref (fun _ -> assert false : Format.formatter -> type_expr -> unit)

(**** Type level management ****)

let generic_level = 100000000

(* Used to mark a type during a traversal. *)
let lowest_level = 0
let pivot_level = 2 * lowest_level - 1
(* pivot_level - lowest_level < lowest_level *)

(**** Some type creators ****)

let new_id = ref (-1)

let newty2 level desc  =
  incr new_id; { desc; level; id = !new_id }
let newgenty desc      = newty2 generic_level desc
let newgenvar ?name () = newgenty (Tvar name)
(*
let newmarkedvar level =
  incr new_id; { desc = Tvar; level = pivot_level - level; id = !new_id }
let newmarkedgenvar () =
  incr new_id;
  { desc = Tvar; level = pivot_level - generic_level; id = !new_id }
*)

(**** Check some types ****)

let is_Tvar = function {desc=Tvar _} -> true | _ -> false
let is_Tunivar = function {desc=Tunivar _} -> true | _ -> false

(**** Representative of a type ****)

let rec repr =
  function
    {desc = Tlink t'} ->
      (*
         We do no path compression. Path compression does not seem to
         improve notably efficiency, and it prevents from changing a
         [Tlink] into another type (for instance, for undoing a
         unification).
      *)
      repr t'
  | t -> t

let rec commu_repr = function
  | Clink r when !r <> Cunknown -> commu_repr !r
  | c -> c

let rec rev_concat l ll =
  match ll with
  | [] -> l
  | l'::ll -> rev_concat (l'@l) ll


let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

let proxy = repr


(**********************************)
(*  Utilities for type traversal  *)
(**********************************)

let iter_type_expr f ty =
  match ty.desc with
    Tvar _              -> ()
  | Tarrow (ty1, ty2, _) -> f ty1; f ty2
  (*   | Ttuple l            -> List.iter f l *)
  (*   | Tconstr (_, l, _)   -> List.iter f l *)
  (*   | Tfield (_, _, ty1, ty2) -> f ty1; f ty2 *)
  | Tnil                -> ()
  | Tlink ty            -> f ty
  | Tsubst ty           -> f ty
  | Tunivar _           -> ()

type type_iterators =
  {
    it_value_description: type_iterators -> value_description -> unit;
    it_path: Path.t -> unit; 
  }

let type_iterators =
  let it_value_description it vd =
    it.it_type_expr it vd.val_type
  and it_do_type_expr it ty =
    iter_type_expr (it.it_type_expr it) ty;
    match ty.desc with
    (*       Tconstr (p, _, _)
             | Tobject (_, {contents=Some (p, _)})
             | Tpackage (p, _, _) ->
             it.it_path p
             | Tvariant row ->
             may (fun (p,_) -> it.it_path p) (row_repr row).row_name *)
    | _ -> ()
  and it_path p = ()
  in
  { it_path; it_type_expr = it_do_type_expr; it_do_type_expr; }

let rec copy_kind = function
    Fvar{contents = Some k} -> copy_kind k
  | Fvar _   -> Fvar (ref None)
  | Fpresent -> Fpresent
  | Fabsent  -> assert false

let copy_commu c =
  if commu_repr c = Cok then Cok else Clink (ref Cunknown)

(* Since univars may be used as row variables, we need to do some
   encoding during substitution *)
let rec norm_univar ty =
  match ty.desc with
  | Tunivar _ | Tsubst _ -> ty
  | Tlink ty           -> norm_univar ty
  | Ttuple (ty :: _)   -> norm_univar ty
  | _                  -> assert false

let rec copy_type_desc ?(keep_names=false) f = function
  | Tvar _ as ty        -> if keep_names then ty else Tvar None
  | Tarrow (p, ty1, ty2, c)-> Tarrow (p, f ty1, f ty2, copy_commu c)
  (* | Ttuple l            -> Ttuple (List.map f l) *)
  (* | Tconstr (p, l, _)   -> Tconstr (p, List.map f l, ref Mnil) *)
  (* | Tnil                -> Tnil *)
  | Tlink ty            -> copy_type_desc f ty.desc
  | Tsubst ty           -> assert false
  | Tunivar _ as ty     -> ty (* always keep the name *)

(* Utilities for copying *)

let saved_desc = ref []
(* Saved association of generic nodes with their description. *)

let save_desc ty desc =
  saved_desc := (ty, desc)::!saved_desc

let saved_kinds = ref [] (* duplicated kind variables *)
let new_kinds = ref []   (* new kind variables *)
let dup_kind r =
  (match !r with None -> () | Some _ -> assert false);
  if not (List.memq r !new_kinds) then begin
    saved_kinds := r :: !saved_kinds;
    let r' = ref None in
    new_kinds := r' :: !new_kinds;
    r := Some (Fvar r')
  end

(* Restored type descriptions. *)
let cleanup_types () =
  List.iter (fun (ty, desc) -> ty.desc <- desc) !saved_desc;
  List.iter (fun r -> r := None) !saved_kinds;
  saved_desc := []; saved_kinds := []; new_kinds := []

(* Mark a type. *)
let rec mark_type ty =
  let ty = repr ty in
  if ty.level >= lowest_level then begin
    ty.level <- pivot_level - ty.level;
    iter_type_expr mark_type ty
  end

let mark_type_node ty =
  let ty = repr ty in
  if ty.level >= lowest_level then begin
    ty.level <- pivot_level - ty.level;
  end

let mark_type_params ty =
  iter_type_expr mark_type ty

let type_iterators =
  let it_type_expr it ty =
    let ty = repr ty in
    if ty.level >= lowest_level then begin
      mark_type_node ty;
      it.it_do_type_expr it ty;
    end
  in
  {type_iterators with it_type_expr}


(* Remove marks from a type. *)
let rec unmark_type ty =
  let ty = repr ty in
  if ty.level < lowest_level then begin
    ty.level <- pivot_level - ty.level;
    iter_type_expr unmark_type ty
  end

let unmark_iterators =
  let it_type_expr it ty = unmark_type ty in
  {type_iterators with it_type_expr}

let unmark_type_decl decl =
  unmark_iterators.it_type_declaration unmark_iterators decl

let unmark_extension_constructor ext =
  List.iter unmark_type ext.ext_type_params;
  List.iter unmark_type ext.ext_args;
  Misc.may unmark_type ext.ext_ret_type

let unmark_class_signature sign =
  unmark_type sign.csig_self;
  Vars.iter (fun l (m, v, t) -> unmark_type t) sign.csig_vars

let unmark_class_type cty =
  unmark_iterators.it_class_type unmark_iterators cty


(*******************************************)
(*  Memorization of abbreviation expansion *)
(*******************************************)

(* Search whether the expansion has been memorized. *)
let rec find_expans priv p1 = function
    Mnil -> None
  | Mcons (priv', p2, ty0, ty, _)
    when priv' >= priv && Path.same p1 p2 -> Some ty
  | Mcons (_, _, _, _, rem)   -> find_expans priv p1 rem
  | Mlink {contents = rem} -> find_expans priv p1 rem

(* debug: check for cycles in abbreviation. only works with -principal
   let rec check_expans visited ty =
   let ty = repr ty in
   assert (not (List.memq ty visited));
   match ty.desc with
    Tconstr (path, args, abbrev) ->
      begin match find_expans path !abbrev with
        Some ty' -> check_expans (ty :: visited) ty'
      | None -> ()
      end
   | _ -> ()
*)

let memo = ref []
(* Contains the list of saved abbreviation expansions. *)

let cleanup_abbrev () =
  (* Remove all memorized abbreviation expansions. *)
  List.iter (fun abbr -> abbr := Mnil) !memo;
  memo := []

let memorize_abbrev mem priv path v v' =
  (* Memorize the expansion of an abbreviation. *)
  mem := Mcons (priv, path, v, v', !mem);
  (* check_expans [] v; *)
  memo := mem :: !memo

let rec forget_abbrev_rec mem path =
  match mem with
    Mnil ->
      assert false
  | Mcons (_, path', _, _, rem) when Path.same path path' ->
      rem
  | Mcons (priv, path', v, v', rem) ->
      Mcons (priv, path', v, v', forget_abbrev_rec rem path)
  | Mlink mem' ->
      mem' := forget_abbrev_rec !mem' path;
      raise Exit

let forget_abbrev mem path =
  try mem := forget_abbrev_rec !mem path with Exit -> ()


(**********************************)
(*  Utilities for backtracking    *)
(**********************************)

type change =
    Ctype of type_expr * type_desc
  | Clevel of type_expr * int
  | Cname of
      (Path.t * type_expr list) option ref * (Path.t * type_expr list) option
  | Crow of row_field option ref * row_field option
  | Ckind of field_kind option ref * field_kind option
  | Ccommu of commutable ref * commutable
  | Cuniv of type_expr option ref * type_expr option
  | Ctypeset of TypeSet.t ref * TypeSet.t

let undo_change = function
    Ctype  (ty, desc) -> ty.desc <- desc
  | Clevel (ty, level) -> ty.level <- level
  | Cname  (r, v) -> r := v
  | Crow   (r, v) -> r := v
  | Ckind  (r, v) -> r := v
  | Ccommu (r, v) -> r := v
  | Cuniv  (r, v) -> r := v
  | Ctypeset (r, v) -> r := v

type changes =
    Change of change * changes ref
  | Unchanged
  | Invalid

type snapshot = changes ref * int

let trail = Weak.create 1
let last_snapshot = ref 0

let log_change ch =
  match Weak.get trail 0 with None -> ()
                            | Some r ->
                                let r' = ref Unchanged in
                                r := Change (ch, r');
                                Weak.set trail 0 (Some r')

let log_type ty =
  if ty.id <= !last_snapshot then log_change (Ctype (ty, ty.desc))
let link_type ty ty' =
  log_type ty;
  let desc = ty.desc in
  ty.desc <- Tlink ty';
  (* Name is a user-supplied name for this unification variable (obtained
   * through a type annotation for instance). *)
  match desc, ty'.desc with
    Tvar name, Tvar name' ->
      begin match name, name' with
      | Some _, None ->  log_type ty'; ty'.desc <- Tvar name
      | None, Some _ ->  ()
      | Some _, Some _ ->
          if ty.level < ty'.level then (log_type ty'; ty'.desc <- Tvar name)
      | None, None   ->  ()
      end
  | _ -> ()
(* ; assert (check_memorized_abbrevs ()) *)
(*  ; check_expans [] ty' *)
let set_level ty level =
  if ty.id <= !last_snapshot then log_change (Clevel (ty, ty.level));
  ty.level <- level
let set_univar rty ty =
  log_change (Cuniv (rty, !rty)); rty := Some ty
let set_name nm v =
  log_change (Cname (nm, !nm)); nm := v
let set_row_field e v =
  log_change (Crow (e, !e)); e := Some v
let set_kind rk k =
  log_change (Ckind (rk, !rk)); rk := Some k
let set_commu rc c =
  log_change (Ccommu (rc, !rc)); rc := c
let set_typeset rs s =
  log_change (Ctypeset (rs, !rs)); rs := s

let snapshot () =
  let old = !last_snapshot in
  last_snapshot := !new_id;
  match Weak.get trail 0 with Some r -> (r, old)
                            | None ->
                                let r = ref Unchanged in
                                Weak.set trail 0 (Some r);
                                (r, old)

let rec rev_log accu = function
    Unchanged -> accu
  | Invalid -> assert false
  | Change (ch, next) ->
      let d = !next in
      next := Invalid;
      rev_log (ch::accu) d

let backtrack (changes, old) =
  match !changes with
    Unchanged -> last_snapshot := old
  | Invalid -> failwith "Btype.backtrack"
  | Change _ as change ->
      cleanup_abbrev ();
      let backlog = rev_log [] change in
      List.iter undo_change backlog;
      changes := Unchanged;
      last_snapshot := old;
      Weak.set trail 0 (Some changes)
