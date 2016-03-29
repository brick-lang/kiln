

(** Helpers to produce Parsetree fragments *)

open Asttypes

type lid = Fqident.t 
type str = string Asttypes.location
type location = Location.t

let default_location = ref Location.none

let with_default_location l f =
  let old = !default_location in
  default_location := l;
  try let r = f () in default_location := old; r
  with exn -> default_location := old; raise exn


module Type = struct
  include ParseTreeNodes.CoreType
  let make ?(location = !default_location) variant = { location; variant; }

  let any   ?location ()  = make ?location  Any
  let var   ?location a   = make ?location (Variable a)
  let lit   ?location a   = make ?location (Literal a)
  let arrow ?location a b = make ?location (Arrow (a, b))
  let tuple ?location a   = make ?location (Tuple a)
end

module Pattern = struct
  include ParseTreeNodes.Pattern
  let make ?(location = !default_location) variant = { location; variant; }

  let any         ?location ()  = make ?location  Any
  let var         ?location a   = make ?location (Variable a)
  let refvar      ?location a   = make ?location (Ref_variable a)
  let alias       ?location a b = make ?location (Alias (a, b))
  let constant    ?location a   = make ?location (Constant a)
  let range       ?location a b = make ?location (Range (a, b))
  let tuple       ?location a   = make ?location (Tuple a)
  let construct   ?location a b = make ?location (Construct (a, b))
  let vector      ?location a   = make ?location (Vector a)
  let or_         ?location a b = make ?location (Or (a, b))
  let constraint_ ?location a b = make ?location (Constraint (a, b))
end

module PatternDefault = struct
  include ParseTreeNodes.PatternDefault
  let make ?(location = !default_location) pattern variant = { pattern; location; variant }

  let default ?location a b  = make ?location a (Default b)
  let none    ?location a    = make ?location a None
end

module Expression = struct
  include ParseTreeNodes.Expression
  let make ?(location = !default_location) variant = { location; variant; }

  let ident       ?location a     = make ?location (Ident a)
  let constant    ?location a     = make ?location (Constant a)
  let let_        ?location a b   = make ?location (Let (a, b))
  let fn_         ?location a b   = make ?location (Function (a, b))
  let apply       ?location a b   = make ?location (Apply (a, b))
  let match_      ?location a b   = make ?location (Match (a, b))
  let tuple       ?location a     = make ?location (Tuple a)
  let field       ?location a b   = make ?location (Field (a, b))
  let vector      ?location a     = make ?location (Vector a)
  let ifelse      ?location a b c = make ?location (Ifelse (a, b, c))
  let sequence    ?location a b   = make ?location (Sequence (a, b))
  let while_      ?location a b   = make ?location (While (a, b))
  let constraint_ ?location a b   = make ?location (Constraint (a, b))
  let assert_     ?location a     = make ?location (Assert a)

  let case lhs ?guard rhs = {
    ParseTreeNodes.Case.lhs = lhs;
    ParseTreeNodes.Case.guard = guard;
    ParseTreeNodes.Case.rhs = rhs;
  }
end

module Structure = struct
  include ParseTreeNodes.Structure
end

module StructureItem = struct
  include ParseTreeNodes.StructureItem
  let make ?(location = !default_location) variant = { location; variant; }
  let eval      ?location a   = make ?location (Eval a)
  let value     ?location a   = make ?location (Value a)
  let using     ?location a b = make ?location (Using (a, b))
  let import    ?location a   = make ?location (Import a)
end

module ValueBinding = struct
  include ParseTreeNodes.ValueBinding
  let make ?(location = !default_location) ?(attrs = []) pattern expression =
    { pattern; expression; location }
end

module FutureBinding = struct
  include ParseTreeNodes.FutureBinding
  let make ?(location = !default_location) ?(attrs = []) pattern expression =
    { pattern; expression; location }
end

module BoundCall = struct
  include ParseTreeNodes.BoundCall
  let make ?(location = !default_location) pattern variant = { pattern; variant; location }
  let pipelined ?location a b = make ?location a (Pipelined b)
  let forked  ?location a b = make ?location a (Forked b)
  let synced  ?location a b = make ?location a (Synced b)
end

module LetStatement = struct
  include ParseTreeNodes.LetStatement
  let make ?(location = !default_location) variant = { variant; location }
  let binding ?location a = make ?location (Binding a)
  let call    ?location a = make ?location (Call a)
  let future  ?location a = make ?location (Future a)
  let import  ?location a = make ?location (Import a)
end;;
