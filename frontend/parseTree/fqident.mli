
(* Fully qualified identifiers for parsetree *)

type t =
  | FQident of string
  | FQdot   of t * string

val flatten: t -> string list
val last: t -> string
val parse: string -> t
