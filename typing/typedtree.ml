(*
 * This file holds information on the parser's
 * internal tree representation after passing
 * through the type checker
 *)

open Asttypes
open Types

type pattern =
  {
    pat_desc:   pattern_desc;
    pat_loc:    Location.t;
    pat_extra:  (pat_extra * Location.t (* * attribute list *)) list;
    pat_type:   type_expr;
    mutable pat_env:  Env.t;
  }

and pat_extra =
  | Tpat_constraint of core_type
  | Tpat_type of Path.t * Fqident.t loc
  | Tpat_unpack

and pattern_desc =
  | Tpat_any
  | Tpat_var       of Ident.t * string loc
  | Tpat_ref_var   of Ident.t * string loc
  | Tpat_constant  of constant
  | Tpat_tuple     of pattern list
  | Tpat_construct of Fqident.t loc * constructor_desc * pattern list
  | Tpat_vector    of pattern list
  | Tpat_or        of pattern * pattern

and expression =
  {
    exp_desc: expression_desc;
    exp_loc:  Location.t;
    exp_extra: (exp_extra * Location.t (* * attribute list *)) list;
    exp_env: Env.t;
  }

and exp_extra =
  | Texp_constraint of core_type

and expression_desc =
  | Texp_ident    of Path.t * Fqident.t loc * Types.value_decription
  | Texp_constant of constant
  (* | Texp_let      of value_binding list * expression *)
  | Texp_fn       of pattern list * partial
  (* | Texp_call     of expression * expression list  *)
(* | Texp_apply    of expression * expression list *)
(* | Texp_match    of expression * case list * case list * partial *)
(* | Texp_tuple    of expression list *)

and case =
  {
    c_lhs: pattern;
    c_guard: expression option;
    c_rhs: expression;
  }

and structure = {
  str_items : structure_item list;
  str_type : Types.signature;
  str_final_env : Env.t;
}

and structure_item =
  { str_desc : structure_item_desc;
    str_loc : Location.t;
    str_env : Env.t
  }

and structure_item_desc =
    Tstr_eval of expression * attributes
  | Tstr_value of rec_flag * value_binding list

and value_binding =
  {
    vb_pat: pattern;
    vb_expr: expression;
    vb_attributes: attributes;
    vb_loc: Location.t;
  }

and 'a include_infos =
  {
    incl_mod: 'a;
    incl_type: Types.signature;
    incl_loc: Location.t;
    incl_attributes: attribute list;
  }

and include_description = module_type include_infos

and include_declaration = module_expr include_infos

and with_constraint =
  | Twith_type of type_declaration
  | Twith_module of Path.t * Longident.t loc
  | Twith_typesubst of type_declaration
  | Twith_modsubst of Path.t * Longident.t loc

and core_type =
  { 
    mutable ctyp_desc : core_type_desc;
    mutable ctyp_type : type_expr;
    ctyp_env : Env.t; (* BINANNOT ADDED *)
    ctyp_loc : Location.t;
    ctyp_attributes: attribute list;
  }
and core_type_desc =
  (* | Ttyp_any *)
  | Ttyp_var     of string
  (* | Ttyp_ref_var of string *)
  | Ttyp_arrow   of label * core_type * core_type
  (* | Ttyp_tuple   of core_type list *)
(* | Ttyp_constr  of Path.t * Longident.t loc * core_type list *)
