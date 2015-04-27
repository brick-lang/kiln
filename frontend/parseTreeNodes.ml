(*
 * This file holds information on the parser's
 * internal tree representation, without context-based
 * information, such as typing
 *)

open Asttypes

(* To avoid confusion, the work 'variant' is used in 
 * this module to refer to the class of AST node,
 * as the words 'type' and 'class' are keywords in
 * OCaml, and the word 'kind' is used in the typing
 * engine. The place holder name 'sort' is used to
 * refer to internal variant types to prevent name
 * clashing, and should only ever be seen in OCaml
 * compiler errors. *)
module rec CoreType : sig
  type t = {
    variant  : sort ;
    location : Location.t;
  }

  and sort =
    (* The "any" type. Aka '_' *)
    | Any

    (* A type variable. Represented like "a" or "t" *)
    | Variable of string

    (* A type literal. Such as Int or String. *)
    | Literal of string

    (* A pipelined applicative type.
     * T1 -> T2
    *)
    | Arrow of t * t

    (* A tuple type is composed of 2 or more inner types
     * (T1, ..., Tn) | where n >= 2
    *)
    | Tuple of t list

    (* This represents a parameterized type
     * constructor such as:
     * Vector<Int>
     * Map<String, (Int, Int)> *)
    | Constructor of Fqident.t location * t list

    | Error

end = CoreType

and Pattern : sig
  type t = {
    variant  : sort;
    location : Location.t;
  }

  and sort =

    (* _ *)
    | Any

    (* A pattern variable is a lexical binding
     * like 'x'. This is what gets used in function
     * prototypes.
     * y : Float
    *)
    | Variable of string location

    (* Like a normal var. Except it points
     * to a ref.
     * !x : Int64
    *)
    | Ref_variable of string location

    (* 1, 'a', "true", 1.0 etc *)
    | Constant of constant

    (* [1 .. 9] *)
    | Range of constant * constant

    (* (P1, ..., Pn) where n >= 2 *)
    | Tuple of t list

    (* P1 | P2 *)
    | Or of t * t

    (* C                 None    -- just the class name
     * C(P)              Some    -- bind the variable
     * C((P1, ..., Pn))  Some (Pattern_tuple [P1; ...; Pn]) *)
    | Construct of Fqident.t location * t option

    (* [P1, ..., Pn] *)
    | Vector of t list

    (* (P : T) *)
    | Constraint of t * CoreType.t

    | Error

end = Pattern


(** Value expressions *)
and Expression : sig
  type t = {
    variant  : sort;
    location : Location.t;
  }

  and sort =

    (* foo
     * Mortar.Collections.BTree.foo *)
    | Ident of Fqident.t location

    (* 1, 'a', "true", 1.0 *)
    | Constant of constant


    (* let P1 = E1
     *     Pn = En
     * in E end    *)
    | Let of ValueBinding.t list * t

    (**
     * Unlike Haskell and the MLs, n-ary functions are not (at the top)
     * represented by a sequence of unary functions. This is because
     * 'calling' and 'currying' are two separate things. A call
     * _gurantees_ that the type of the evaluated expression is the
     * 'return' type. While currying gurantees that we move to the
     * n-th type in the type sequence when applying n arguments. *)
    (* |P| ([P], E)
    *)

    (* TODO: we need to somehow check for pattern variables
     * so that calling like foo{x:3} always works *)
    | Function of Pattern.t list * t

    (**
     * Function fragments are what functions are *actually*
     * made of. They are what makes currying possible. Basically
     * they are the base functions in the λ-calculus *)
    | Function_fragment of Pattern.t * t

    (* E0(E1, ..., En) *)
    | Call of t * t list

    (* E0[E1, ..., En] *)
    | Apply of t * t list

    (* E0{ l1:E, ..., ln:E } *)
    (* | Expr_named_call of expression * (label * expression) list *)

    (* E0 => E1 *)
    | Parallel_call of t * t

    (* E0 ~> E1 *)
    | Synch_call of t * t

    (* E0 -> E1 *)
    | Pipeline_call of t * t

    (* match E0
     *   | P1 -> E1
     *   | ...
     *   | Pn -> En
    *)
    | Match of t * Case.t list (* This was (case list) *)

    (* (E1, ..., En)   (n >= 2) *)
    | Tuple of t list

    (* e.l *)
    | Field of t * Fqident.t location

    (* [ E0, ..., En ] *)
    | Vector of t list

    (* if E1; E2; else E3; end *)
    | Ifelse of t * t * t option

    (* E1; E2
     * E1 NEWLINE E2 *)
    | Sequence of t * t

    (* while E1
     *   E2     *)
    | While of t * t

    (* (E : T) *)
    | Constraint of t * CoreType.t

    (* assert E
     * Note: "assert false" is treated in a special way by the
     * type-checker. *)
    | Assert of t

    | Error

end = Expression

and Case : sig
  (* (P -> E) or (P when E0 -> E) *)
  type t = {
    lhs   : Pattern.t;
    guard : Expression.t option;
    rhs   : Expression.t;
  }
end = Case

and Structure : sig
  type t = StructureItem.t list
end = Structure

and StructureItem : sig
  type t = {
    variant  : sort;
    location : Location.t;
  }

  and sort =
    (* E *)
    | Eval  of Expression.t

    (* let P1 = E1 *)
    | Value of ValueBinding.t

    | Error

end = StructureItem


(* A value binding is used wherever we associate an
 * expression with a name. Functions and variables
 * are examples of bound values.
 * main { 4 } *)
and ValueBinding : sig
  type t = {
    pattern    : Pattern.t;
    expression : Expression.t;
    location   : Location.t;
  }
end = ValueBinding

(* A let value binding is a specialization that can
 * include a bound call
 * y = 4
 *   -> foo(2)
*)
and LetValueBinding : sig
  type t = {
    pattern    : Pattern.t;
    expression : Expression.t;
    location   : Location.t;

    (* Only non-top-level let expressions can have bound calls *)
    call       : BoundCall.t;
  }
end = LetValueBinding

and  BoundCall : sig
  type t = {
    location : Location.t;
    variant  : sort;
  }

  and sort =
    (* -> foo(23) *)
    | Pipelined of Expression.t

    (* => foo(23) *)
    | Forked    of Expression.t

    (* ~> foo(23) *)
    | Synced    of Expression.t

    | None

end = BoundCall
