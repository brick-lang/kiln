
(* This file represents "fully qualified" identifiers *)
(* e.g. "Mortar.Collections.BTree.insert" *)

open Core.Std

type t =
  | FQident of string
  | FQdot of t * string

let flatten lid =
  let rec flat accu = function
    | FQident s     -> s :: accu
    | FQdot(lid, s) -> flat (s :: accu) lid
  in
  flat [] lid

let last = function
  | FQident s   -> s
  | FQdot(_, s) -> s

let rec split_at_dots s pos =
  try
    let dot = String.index_from_exn s pos '.' in (* TODO: make this use the option type for safety *)
    String.sub s pos (dot - pos) :: split_at_dots s (dot + 1)
  with Not_found ->
    [String.sub s pos (String.length s - pos)]

let parse s =
  match split_at_dots s 0 with
  | [] -> FQident ""  (* should not happen, but don't put assert false
                         so as not to crash the toplevel (see Genprintval) *)
  | hd :: tl -> List.fold_left ~init:(FQident hd) ~f:(fun p s -> FQdot(p, s)) tl
