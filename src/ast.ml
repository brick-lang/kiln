open Core.Std

type type_annot = string option

type variable = 
    | Ref_Var of string * type_annot
    | Plain_Var of string * type_annot

(* The function prototype (declaration) *)
type proto = Prototype of variable list

type expr = 
  | Integer  of int * type_annot
  | Float    of float * type_annot
  (* | Binary  of string * expr * expr *)
  | Bool     of bool * type_annot
  | Ident    of string
  | Block    of expr list * type_annot
  | Function of string * variable list * expr
  | Call     of string * variable list

type bound_call =
  | Forked_Call of expr
  | Pipelined_Call of expr
  | Synced_Call of expr
  | No_Call

type binding = Binding of variable * expr * bound_call (* var = expr *)

type stmt =
  | Top  of expr
  | Let  of binding list * expr (* binding list, body *)
  | Sep 
  | End
