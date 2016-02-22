
(** 
 * These are the constants we can actually 
 * store in the AST. Things like `2`, or "foo".
*)
type constant =
  | Const_int    of int
  | Const_string of string * string option
  | Const_float  of float
  | Const_int32  of int32
  | Const_int64  of int64

type 'a location = 'a Location.loc = {
  txt : 'a;
  loc : Location.t;
}

type label = string
