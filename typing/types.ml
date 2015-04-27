open Asttypes

type type_expr =
  { 
    mutable desc: type_desc;
    mutable level: int;
    mutable id: int 
  }

and type_desc =
  | Tvar of string option
  | Tarrow of type_expr * type_expr * commutable
  (* | Ttuple of type_expr list *)
  | Tnil
  | Tlink of type_expr
  | Tsubst of type_expr         (* for copying *)
  | Tunivar of string option

and commutable =
  | Cok
  | Cunknown
  | Clink of commutable ref

(* Basic operations on type expressions *)
module TypeOps = struct
  type t = type_expr
  let compare t1 t2 = t1.id - t2.id
  let hash t = t.id
  let equal t1 t2 = t1 == t2
end

(* Value descriptions *)

type value_description =
  { 
    val_type: type_expr;                (* Type of the value *)
    val_kind: value_kind;
    val_loc: Location.t;
  }

and value_kind =
  | Val_reg                             (* Regular value *)
  | Val_prim of Primitive.description   (* Primitive *)
  | Val_unbound                         (* Unbound variable *)
