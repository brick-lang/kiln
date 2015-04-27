(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Auxiliary type for reporting syntax errors *)

type error =
  | Unmatched of Location.t * string * Location.t * string
  | Expecting of Location.t * string
  | Not_expecting of Location.t * string
  | Applicative_path of Location.t
  | Variable_in_scope of Location.t * string
  | Other of Location.t

exception Error of error
exception Escape_error

let errors : error Core.Std.Queue.t = Core.Std.Queue.create ();;

let prepare_error error = 
  let errorf = Location.errorf ~header:"Syntax Error" in
  match error with
  | Unmatched(opening_loc, opening, closing_loc, closing) ->
      errorf ~location:closing_loc
        ~sub_errors:[
          errorf ~location:opening_loc
            "This '%s' might be unmatched" opening
        ]
        "'%s' expected" closing

  | Expecting (location, nonterm) ->
      errorf ~location "Expected %s." nonterm

  | Not_expecting (location, nonterm) ->
      errorf ~location "Unexpected %s." nonterm

  | Applicative_path location ->
      Location.error ~header:"Syntax Error" ~location
        "applicative paths of the form F(X).t \
         are not supported when the option -no-app-func is set."

  | Variable_in_scope (location, var) ->
      Location.error ~header:"Syntax Error" ~location
        (Format.sprintf
           "In this scoped type, variable '%s is reserved for the local type %s." var var)

  | Other location ->
      Location.error ~header:"Syntax Error" ~location ""

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (prepare_error err)
      | _ -> None
    )

let location = function
  | Unmatched(l,_,_,_)
  | Applicative_path l
  | Variable_in_scope(l,_)
  | Other l
  | Not_expecting (l, _)
  | Expecting (l, _) -> l