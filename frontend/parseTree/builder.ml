

(** Helpers to produce Parsetree fragments *)

type lid = Fqident.t 
type str = string Location.loc
type location = Location.t

let default_location = ref Location.none

let with_default_location l f =
  let old = !default_location in
  default_location := l;
  try
    let r = f () in
    default_location := old;
    r
  with exn ->
    default_location := old;
    raise exn


module Type = struct
  include Nodes.CoreType
  let make ?(loc = !default_location) variant = { location=loc; variant; }

  let any   ?loc ()  = make ?loc  Any
  let var   ?loc a   = make ?loc (Variable a)
  let lit   ?loc a   = make ?loc (Literal a)
  let arrow ?loc a b = make ?loc (Arrow (a, b))
  let tuple ?loc a   = make ?loc (Tuple a)
  let constructor ?loc a b = make ?loc (Constructor (a,b))
end

module Pattern = struct
  include Nodes.Pattern
  let make ?(loc = !default_location) variant = { location=loc; variant; }

  let any         ?loc ()  = make ?loc  Any
  let var         ?loc a   = make ?loc (Variable a)
  let refvar      ?loc a   = make ?loc (Ref_variable a)
  let alias       ?loc a b = make ?loc (Alias (a, b))
  let constant    ?loc a   = make ?loc (Constant a)
  let range       ?loc a b = make ?loc (Range (a, b))
  let tuple       ?loc a   = make ?loc (Tuple a)
  let construct   ?loc a b = make ?loc (Construct (a, b))
  let vector      ?loc a   = make ?loc (Vector a)
  let or_         ?loc a b = make ?loc (Or (a, b))
  let constraint_ ?loc a b = make ?loc (Constraint (a, b))
end

module PatternDefault = struct
  include Nodes.PatternDefault
  let make ?(loc = !default_location) pattern variant = { pattern; location=loc; variant }

  let default ?loc a b  = make ?loc a (Default b)
  let none    ?loc a    = make ?loc a None
end

module Expression = struct
  include Nodes.Expression
  let make ?(loc = !default_location) variant = { location=loc; variant; }

  let ident       ?loc a     = make ?loc (Ident a)
  let constant    ?loc a     = make ?loc (Constant a)
  let let_        ?loc a b   = make ?loc (Let (a, b))
  let fn          ?loc a b   = make ?loc (Function (a, b))
  let call        ?loc a b   = make ?loc (Call (a, b))
  let apply       ?loc a b   = make ?loc (Apply (a, b))
  let match_      ?loc a b   = make ?loc (Match (a, b))
  let tuple       ?loc a     = make ?loc (Tuple a)
  let field       ?loc a b   = make ?loc (Field (a, b))
  let vector      ?loc a     = make ?loc (Vector a)
  let ifelse      ?loc a b c = make ?loc (Ifelse (a, b, c))
  let sequence    ?loc a b   = make ?loc (Sequence (a, b))
  let while_      ?loc a b   = make ?loc (While (a, b))
  let constraint_ ?loc a b   = make ?loc (Constraint (a, b))
  let assert_     ?loc a     = make ?loc (Assert a)

  let case lhs ?guard rhs = {
    Nodes.Case.lhs = lhs;
    Nodes.Case.guard = guard;
    Nodes.Case.rhs = rhs;
  }
end

module Structure = struct
  include Nodes.Structure
end

module StructureItem = struct
  include Nodes.StructureItem
  let make ?(loc = !default_location) variant = { location=loc; variant; }
  let eval      ?loc a   = make ?loc (Eval a)
  let value     ?loc a   = make ?loc (Value a)
  let using     ?loc a b = make ?loc (Using (a, b))
  let import    ?loc a   = make ?loc (Import a)
end

module ValueBinding = struct
  include Nodes.ValueBinding
  let make ?(loc = !default_location) ?(attrs = []) pattern expression =
    { pattern; expression; location=loc }
end

module FutureBinding = struct
  include Nodes.FutureBinding
  let make ?(loc = !default_location) ?(attrs = []) pattern expression =
    { pattern; expression; location=loc }
end

module BoundCall = struct
  include Nodes.BoundCall
  let make ?(loc = !default_location) pattern variant = { pattern; variant; location=loc }
  let pipelined ?loc a b = make ?loc a (Pipelined b)
  let forked  ?loc a b = make ?loc a (Forked b)
  let synced  ?loc a b = make ?loc a (Synced b)
end

module LetStatement = struct
  include Nodes.LetStatement
  let make ?(loc = !default_location) variant = { variant; location=loc }
  let binding ?loc a = make ?loc (Binding a)
  let call    ?loc a = make ?loc (Call a)
  let future  ?loc a = make ?loc (Future a)
  let import  ?loc a = make ?loc (Import a)
end

module Constant = struct
  include Nodes.Constant
  let make ?(loc = !default_location) variant = { variant; location=loc }
  let int    ?loc a  = make ?loc (Int a)
  let string ?loc a  = make ?loc (String a)
  let float  ?loc a  = make ?loc (Float a)
  let int32  ?loc a  = make ?loc (Int32 a)
  let int64  ?loc a  = make ?loc (Int64 a)
  let true_  ?loc () = make ?loc (True)
  let false_ ?loc () = make ?loc (False)
  let unit   ?loc () = make ?loc (Unit)
end
