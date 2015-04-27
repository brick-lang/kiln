open Asttypes
open ParseTreeNodes
open Sedlexing
open Format
open Location
open Yojson.Basic

let fmt_position with_name l =
  let fname = if with_name then l.file_name else "" in
  if l.line_number = -1
  then sprintf "%s[%d]" fname l.buffer_offset
  else sprintf "%s[%d,%d+%d]" fname l.line_number l.line_offset
         (l.buffer_offset - l.line_offset)
;;

let rec fmt_fqident_aux = function
  | Fqident.FQident (s) -> sprintf "%s" s;
  | Fqident.FQdot (y,s) -> sprintf "%s.%s" (fmt_fqident_aux y) s;
;;

let location loc =
  let p_2nd_name = loc.loc_start.file_name <> loc.loc_end.file_name in
  let value = (sprintf "(%s..%s)" 
                 (fmt_position true loc.loc_start)
                 (fmt_position p_2nd_name loc.loc_end)) ^ 
              (if loc.loc_ghost then " ghost" else "") in
  "location", `String value
;;

let fqident_loc (x : Fqident.t location) : json = 
  `Assoc [
    location x.loc;
    "fqident", `String (fmt_fqident_aux x.txt);
  ]
;;

let string_loc (x : string location) : json =
  `Assoc [
    location x.loc;
    "string",`String x.txt; 
  ]
;;

let fmt_constant (x:constant) : json =
  let ls = 
    match x with
    | Const_int (i) -> 
        ["kind", `String "int"; "value", `Int i]
    | Const_string (s, _) -> 
        ["kind", `String "string"; "data", `String s]
    | Const_float (f) ->
        ["kind", `String "float"; "data", `String f]
    | Const_int32 (i) ->
        ["kind", `String "int32"; "data", `Int (Int32.to_int i)]
    | Const_int64 (i) ->
        ["kind", `String "int64"; "data", `Int (Int64.to_int i)]
  in
  `Assoc ls;
;;

(* let fmt_private_flag f x = *)
(*   match x with *)
(*   | Public -> fprintf f "Public"; *)
(*   | Private -> fprintf f "Private"; *)
(* ;; *)

let list (f:'a -> json) : 'a list -> json = function 
  | [] -> `List []
  | l -> `List (List.map f l)
;;

let option f : 'a option -> json = function
  | None -> `Null;
  | Some x -> f x;
;;

let string s : json = `String s;;
let bool x : json = `Bool x;;

let rec core_type (x:CoreType.t) : json =
  let ls = 
    (location x.CoreType.location) ::
    match x.CoreType.variant with
    | CoreType.Any -> 
        ["kind", `String "any"];

    | CoreType.Variable (s) ->  
        ["kind", `String "var"; 
         "string", `String s];

    | CoreType.Literal (s) -> 
        ["kind", `String "lit"; 
         "string", `String s];

    | CoreType.Arrow (ct1, ct2) ->
        ["kind", `String "arrow"; 
         "from_type", core_type ct1;
         "to_type", core_type ct2];

    | CoreType.Tuple l ->
        ["kind", `String "tuple"; 
         "core_types", list core_type l];

    | CoreType.Constructor (li, tl) ->
        ["kind", `String "constr"; 
         "fqident", fqident_loc li;
         "core_types", list core_type tl];

    | CoreType.Error ->
        ["kind", `String "error"]
  in
  `Assoc ls
and pattern (x:Pattern.t) =
  let ls = 
    (location x.Pattern.location) ::
    match x.Pattern.variant with
    | Pattern.Any -> 
        ["kind", `String "any"];

    | Pattern.Variable (s) -> 
        ["kind", `String "var"; 
         "string loc", string_loc s];      

    | Pattern.Ref_variable (s) -> 
        ["kind", `String "ref_var"; 
         "string loc", string_loc s];       

    | Pattern.Constant (c) -> 
        ["kind", `String "constant"; 
         "constant", fmt_constant c];  

    | Pattern.Range (c1, c2) ->
        ["kind", `String "interval"; 
         "from", fmt_constant c1;
         "to", fmt_constant c2];

    | Pattern.Tuple (l) ->
        ["kind", `String "tuple"; 
         "patterns", list pattern l];

    | Pattern.Construct (li, po) ->
        ["kind", `String "constr"; 
         "fqident", fqident_loc li;
         "pattern_option", option pattern po];

    | Pattern.Vector (l) ->
        ["kind", `String "vector";
         "patterns", list pattern l];

    | Pattern.Or (p1, p2) ->
        ["kind", `String "or";
         "left_pattern", pattern p1;
         "right_pattern", pattern p2];

    | Pattern.Constraint (p, ct) ->
        ["constructor", `String "constraint";
         "core_type", core_type ct;
         "pattern", pattern p];

    | Pattern.Error ->
        ["kind", `String "error"]
  in
  `Assoc ls
and expression (x:Expression.t) =
  let ls =
    (location x.Expression.location) ::
    match x.Expression.variant with
    | Expression.Ident (li) -> 
        ["kind", `String "ident";
         "fqident loc", fqident_loc li]

    | Expression.Constant (c) -> 
        ["kind", `String "constant";
         "constant", fmt_constant c]

    | Expression.Let (l, e) ->
        ["kind", `String "let";
         "bindings", list value_binding l;
         "expression", expression e]

    | Expression.Function (l, e) ->
        ["kind", `String "fn";
         "patterns", list pattern l;
         "expression", expression e]

    | Expression.Function_fragment (p, e) ->
        ["kind", `String "fn_fragment";
         "pattern", pattern p;
         "expression", expression e]

    | Expression.Call (e, l) ->
        ["kind", `String "call";
         "expression", expression e;
         "arguments", list expression l]

    | Expression.Apply (e, l) ->
        ["kind", `String "apply";
         "expression", expression e;
         "arguments", list expression l]

    | Expression.Parallel_call (e, f) ->
        ["kind", `String "parallel_call";
         "expression", expression e;
         "argument", expression f]

    | Expression.Synch_call (e, f) ->
        ["kind", `String "synch_call";
         "expression", expression e;
         "argument", expression f]

    | Expression.Pipeline_call (e, f) ->
        ["kind", `String "synch_call";
         "expression", expression e;
         "argument", expression f]

    | Expression.Match (e, l) ->
        ["kind", `String "match";
         "expression", expression e]

    | Expression.Tuple (l) ->
        ["kind", `String "fn";
         "expressions", list expression l]

    | Expression.Field (e, li) ->
        ["kind", `String "field";
         "fqident loc", fqident_loc li;
         "expression", expression e]

    | Expression.Vector (l) ->
        ["kind", `String "vector";
         "expressions", list expression l]

    | Expression.Ifelse (e1, e2, eo) ->
        ["kind", `String "if else";
         "condition", expression e1;
         "body", expression e2;
         "else", option expression eo]

    | Expression.Sequence (e1, e2) ->
        ["kind", `String "sequence";
         "body", expression e1;
         "next", expression e2]

    | Expression.While (e1, e2) ->
        ["kind", `String "while";
         "condition", expression e1;
         "body", expression e2]

    | Expression.Constraint (e, ct) ->
        ["kind", `String "constraint";
         "core_type", core_type ct;
         "expression", expression e]

    | Expression.Assert (e) ->
        ["kind", `String "assert";
         "expression", expression e]

    |Expression.Error ->
        ["kind", `String "error";]
  in
  `Assoc ls

and type_parameter (x, _variance) = core_type x

and structure x = list structure_item x

and structure_item x =
  let ls = 
    (location x.StructureItem.location) ::
    match x.StructureItem.variant with
    | StructureItem.Eval e ->
        ["kind", `String "eval";
         "expression", expression e]

    | StructureItem.Value (l) ->
        ["kind", `String "value";
         "value_binding", value_binding l]

    | StructureItem.Error ->
        ["kind", `String "error"]
  in `Assoc ls

and value_binding x =
  `Assoc [
    "pattern", pattern x.ValueBinding.pattern ;
    "expression", expression x.ValueBinding.expression
  ]

let implementation (x:Structure.t) : json = structure x;;