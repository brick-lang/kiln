open Nodes
open Sedlexing
open Format
open Common.Location

let fmt_position with_name f l =
  let fname = if with_name then l.file_name else "" in
  if l.line_number = -1
  then fprintf f "%s[%d]" fname l.buffer_offset
  else fprintf f "%s[%d,%d+%d]" fname l.line_number l.line_offset
         (l.buffer_offset - l.line_offset)
;;

let rec fmt_fqident_aux f = function
  | Fqident.FQident (s) -> fprintf f "%s" s;
  | Fqident.FQdot (y,s) -> fprintf f "%a.%s" fmt_fqident_aux y s;
;;

let fmt_location f loc =
  let p_2nd_name = loc.loc_start.file_name <> loc.loc_end.file_name in
  fprintf f "(%a..%a)" (fmt_position true) loc.loc_start
    (fmt_position p_2nd_name) loc.loc_end;
  if loc.loc_ghost then fprintf f " ghost";
;;

let fmt_fqident_loc f x = 
  fprintf f "\"%a\" %a" fmt_fqident_aux x.txt fmt_location x.loc
;;

let fmt_fqident f (x :Fqident.t) = 
  fprintf f "\"%a\"" fmt_fqident_aux x
;;

let fmt_string_loc f x =
  fprintf f "\"%s\" %a" x.txt fmt_location x.loc;
;;

let fmt_constant f c = match c.Constant.variant with
  | Constant.Int (i) -> fprintf f "Const_int %d" i;
      (* | Const_char (c) -> fprintf f "Const_char %02x" (Char.code c); *)
  | Constant.String (s) -> fprintf f "Const_string(%S)" s;
  | Constant.Float (i) -> fprintf f "Const_float %f" i;
  | Constant.Int32 (i) -> fprintf f "Const_int32 %ld" i;
  | Constant.Int64 (i) -> fprintf f "Const_int64 %Ld" i;
  | Constant.False -> fprintf f "Const_false false";
  | Constant.True -> fprintf f "Const_true true"
  | Constant.Unit -> fprintf f "Const_unit unit"
;;

(* let fmt_private_flag f x = *)
(*   match x with *)
(*   | Public -> fprintf f "Public"; *)
(*   | Private -> fprintf f "Private"; *)
(* ;; *)

let line i f s (*...*) =
  fprintf f "%s" (String.make ((2*i) mod 72) ' ');
  fprintf f s (*...*)
;;

let list i f ppf l =
  match l with
  | [] -> line i ppf "[]\n";
  | _ :: _ ->
      line i ppf "[\n";
      List.iter (f (i+1) ppf) l;
      line i ppf "]\n";
;;

let option i f ppf x =
  match x with
  | None -> line i ppf "None\n";
  | Some x ->
      line i ppf "Some\n";
      f (i+1) ppf x;
;;

let fqident_loc i ppf li = line i ppf "%a\n" fmt_fqident_loc li;;
let string i ppf s = line i ppf "\"%s\"\n" s;;
let string_loc i ppf s = line i ppf "%a\n" fmt_string_loc s;;
let bool i ppf x = line i ppf "%s\n" (string_of_bool x);;
let label i ppf x = line i ppf "label=\"%s\"\n" x;;

let rec core_type i ppf (x:CoreType.t) =
  line i ppf "core_type %a\n" fmt_location x.CoreType.location;
  let i = i+1 in
  match x.CoreType.variant with
  | CoreType.Any -> line i ppf "CoreType.Any\n";

  | CoreType.Variable (s) -> line i ppf "CoreType.Variable %s\n" s;

  | CoreType.Literal (s) -> line i ppf "CoreType.Literal %s\n" s;

  | CoreType.Arrow (ct1, ct2) ->
      line i ppf "CoreType.Arrow\n";
      core_type i ppf ct1;
      core_type i ppf ct2;

  | CoreType.Tuple l ->
      line i ppf "CoreType.Tuple\n";
      list i core_type ppf l;

  | CoreType.Constructor (li, tl) ->
      line i ppf "CoreType.Constructor\n";
      line i ppf "%a\n" fmt_fqident li;
      list i core_type ppf tl;

  | CoreType.Error ->
      line i ppf "CoreType.Err\n";

and pattern i ppf (x:Pattern.t) =
  line i ppf "pattern %a\n"  fmt_location x.Pattern.location;
  let i = i+1 in
  match x.Pattern.variant with
  | Pattern.Any -> line i ppf "Pattern.Any\n";

  | Pattern.Variable (s) -> line i ppf "Pattern.Variable %s\n" s;

  | Pattern.Ref_variable (s) -> line i ppf "Pattern.Ref_variable %s\n" s;

  | Pattern.Alias (p1, s) ->
      line i ppf "Pattern.Alias\n";
      pattern i ppf p1;
      line i ppf "%s\n" s;


  | Pattern.Constant (c) -> line i ppf "Pattern.Constant %a\n" fmt_constant c;

  | Pattern.Range (c1, c2) ->
      line i ppf "Pattern.Interval %a..%a\n" fmt_constant c1 fmt_constant c2;

  | Pattern.Tuple (l) ->
      line i ppf "Pattern.Tuple\n";
      list i pattern ppf l;

  | Pattern.Construct (li, po) ->
      line i ppf "Pattern.Construct %a\n" fmt_fqident_loc li;
      option i pattern ppf po;

  | Pattern.Vector (l) ->
      line i ppf "Pattern.Array\n";
      list i pattern ppf l;

  | Pattern.Or (p1, p2) ->
      line i ppf "Pattern.Or\n";
      pattern i ppf p1;
      pattern i ppf p2;

  | Pattern.Constraint (p, ct) ->
      line i ppf "Pattern.Constraint\n";
      pattern i ppf p;
      core_type i ppf ct; 

  | Pattern.Error ->
      line i ppf "Pattern.Error\n";

and pattern_default i ppf (x:PatternDefault.t) =
  let open PatternDefault in
  pattern i ppf x.pattern;
  match x.variant with
  | None -> ()
  | Default d -> 
      line i ppf "default\n";
      expression (i+1) ppf d

and expression i ppf (x:Expression.t) =
  line i ppf "expression %a\n" fmt_location x.Expression.location;
  let i = i+1 in
  match x.Expression.variant with
  | Expression.Ident (li) -> line i ppf "Expression.Ident %a\n" fmt_fqident li;

  | Expression.Constant (c) -> line i ppf "Expression.Constant %a\n" fmt_constant c;

  | Expression.Let (l,e) ->
      line i ppf "Expression.Let\n";
      list i let_ ppf l;
      expression i ppf e

  | Expression.Function (l, e(* , cto *)) ->
      line i ppf "Expression.Function\n" ;
      (*       option i core_type ppf cto; *)
      list i pattern_default ppf l;
      expression i ppf e;

  | Expression.Function_fragment (p,e) ->
      line i ppf "Expression.Function_fragment\n";
      pattern i ppf p;
      expression i ppf e;

  | Expression.Apply (e, l) ->
      line i ppf "Expression.Apply\n";
      expression i ppf e;
      list i expression ppf l;

  | Expression.Call (e, l) ->
      line i ppf "Expression.Call\n";
      expression i ppf e;
      list i expression ppf l;


  | Expression.Parallel_call (e, f) ->
      line i ppf "Expression.Parallel_call\n";
      expression i ppf e;
      expression i ppf f;


  | Expression.Sync_call (e, f) ->
      line i ppf "Expression.Synch_call\n";
      expression i ppf e;
      expression i ppf f;

  | Expression.Pipeline_call (e, f) ->
      line i ppf "Expression.Pipeline_call\n";
      expression i ppf e;
      expression i ppf f;

  | Expression.Match (e, l) ->
      line i ppf "Expression.Match\n";
      expression i ppf e;
      (* list i case ppf l; *)

  | Expression.Tuple (l) ->
      line i ppf "Expression.Tuple\n";
      list i expression ppf l;

  | Expression.Field (e, li) ->
      line i ppf "Expression.Field\n";
      expression i ppf e;
      fqident_loc i ppf li;

  | Expression.Vector (l) ->
      line i ppf "Expression.Vector\n";
      list i expression ppf l;

  | Expression.Ifelse (e1, e2, eo) ->
      line i ppf "Expression.Ifthenelse\n";
      expression i ppf e1;
      expression i ppf e2;
      option i expression ppf eo;

  | Expression.Sequence (e1, e2) ->
      line i ppf "Expression.Sequence\n";
      expression i ppf e1;
      expression i ppf e2;

  | Expression.While (e1, e2) ->
      line i ppf "Expression.While\n";
      expression i ppf e1;
      expression i ppf e2;

  | Expression.Constraint (e, ct) ->
      line i ppf "Expression.Constraint\n";
      expression i ppf e;
      core_type i ppf ct;

  | Expression.Assert (e) ->
      line i ppf "Expression.Assert\n";
      expression i ppf e;


  | Expression.Error -> 
      line i ppf "Expression.Error\n";

and type_parameter i ppf (x, _variance) = core_type i ppf x

and structure i ppf x = list i structure_item ppf x

and structure_item i ppf (x:StructureItem.t) =
  line i ppf "structure_item %a\n" fmt_location x.StructureItem.location;
  let i = i+1 in
  match x.StructureItem.variant with
  | StructureItem.Eval e ->
      line i ppf "StructureItem.Eval\n";
      expression i ppf e;

  | StructureItem.Value (l) ->
      line i ppf "StructureItem.Value\n";
      value_binding i ppf l;

  | StructureItem.Using (t, v) -> 
      line i ppf "StructureItem.Using\n";
      core_type i ppf t;
      string i ppf v;

  | StructureItem.Import t ->
      line i ppf "StructureItem.Import\n";
      core_type i ppf t

  | StructureItem.Module s ->
      line i ppf "StructureItem.Module\n";
      list i structure_item ppf s

  | StructureItem.Error ->
      line i ppf "StructureItem.Error\n"


and value_binding i ppf (x:ValueBinding.t) =
  line i ppf "<val>\n";
  pattern (i+1) ppf x.ValueBinding.pattern;
  expression (i+1) ppf x.ValueBinding.expression

and future_binding i ppf (x:FutureBinding.t) =
  line i ppf "<future>\n";
  pattern (i+1) ppf x.FutureBinding.pattern;
  expression (i+1) ppf x.FutureBinding.expression;

and bound_call i ppf (x:BoundCall.t) =
  line i ppf "<call>\n";
  let i = i+1 in
  pattern i ppf x.BoundCall.pattern;
  match x.BoundCall.variant with
  | BoundCall.Forked e ->
      line i ppf "BoundCall.Forked\n";
      expression i ppf e

  | BoundCall.Synced e ->
      line i ppf "BoundCall.Synced\n";
      expression i ppf e

  | BoundCall.Pipelined e ->
      line i ppf "BoundCall.Pipelined\n";
      expression i ppf e


and let_ i ppf (x:LetStatement.t) =
  line i ppf "let_struct %a\n" fmt_location x.LetStatement.location;
  let i = i+1 in
  match x.LetStatement.variant with
  | LetStatement.Binding b ->
      line i ppf "LetStatement.Binding\n";
      value_binding i ppf b

  | LetStatement.Call c ->
      line i ppf "LetStatement.Call\n";
      bound_call i ppf c

  | LetStatement.Future f ->
      line i ppf "LetStatement.Future\n";
      future_binding i ppf f

  | LetStatement.Import t ->
      line i ppf "LetStatement.Import\n";
      core_type i ppf t

let implementation ppf x = list 0 structure_item ppf x;;
