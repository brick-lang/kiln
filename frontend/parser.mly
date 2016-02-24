%{
open Lexing
open ParseTree
open Asttypes
module Error = ParserError

let symbol_rloc start_pos end_pos = {
  Location.loc_start = Sedlexing.pos_sedlexing start_pos;
  Location.loc_end   = Sedlexing.pos_sedlexing end_pos;
  Location.loc_ghost = false;
};;

let symbol_gloc start_pos end_pos = {
  Location.loc_start = Sedlexing.pos_sedlexing start_pos;
  Location.loc_end   = Sedlexing.pos_sedlexing end_pos;
  Location.loc_ghost = true;
};;

let make_type       d sp ep = Type.make          ~location:(symbol_rloc sp ep) d
let make_pattern    d sp ep = Pattern.make       ~location:(symbol_rloc sp ep) d
let make_expression d sp ep = Expression.make    ~location:(symbol_rloc sp ep) d
let make_structure  d sp ep = StructureItem.make ~location:(symbol_rloc sp ep) d

let mkrhs rhs sp ep = Location.mkloc rhs (symbol_rloc sp ep)
let reloc_pat x sp ep = { x with Pattern.location = symbol_rloc sp ep };;
let reloc_exp x sp ep = { x with Expression.location = symbol_rloc sp ep };;

let make_patternvar name sp ep =
  Pattern.make ~location:(symbol_rloc sp ep) (Pattern.Variable (mkrhs name sp ep))

let make_structureexp e = {
  StructureItem.variant = StructureItem.Value e;
  StructureItem.location = e.ValueBinding.location
}

let unclosed opening_name opening_sp opening_ep closing_name closing_sp closing_ep =
  Core.Std.Queue.enqueue Error.errors (Error.Unmatched(symbol_rloc opening_sp opening_ep, opening_name,
				                              symbol_rloc closing_sp closing_ep, closing_name))

let unexpected ?(suggestion="") name opening closing  =
  Core.Std.Queue.enqueue Error.errors (Error.Not_expecting(symbol_rloc opening closing, name, suggestion))

let expected ?(suggestion="") name opening closing =
  Core.Std.Queue.enqueue Error.errors (Error.Expecting(symbol_rloc opening closing, name, suggestion))

%}

(* TOKENS *)
%token  BEGIN
%token  ASSERT
%token  CLASS
%token  COND
%token  ELSE
%token  END
%token  IF
%token  IN
%token  MATCH
%token  TRAIT
%token  METHOD
%token  MUTATOR
%token  SYNCED
%token  PARALLEL
%token  LET      (* variable binding*)
%token  FUNCTION (* function creation *)
%token  RETURN

(* ops *)
%token  PIPE           (* | *)
%token  COMMA          (* , *)
%token  QUOTE
%token  EQUAL          (* = *)
%token  LESS_THAN      (* < *)
%token  GREATER_THAN   (* > *)
%token  DOUBLE_EQUALS  (* == *)
%token  SPACESHIP      (* <=> *)
%token  BANG           (* ! *)
%token  PLUS           (* + *)
%token  MINUS          (* - *)
%token  ASTERISK       (* * *)
%token  PERCENT        (* % *)
%token  FWD_SLASH      (* / *)
%token  BCK_SLASH      (* \ *)
%token  UNDERSCORE     (* _ *)
%token  AND            (* && | and *)
%token  OR             (* || | or *)
%token  NOT            (* not *)
%token  DOT            (* . *)

(* Whitespace *)
%token NEWLINE

(* Arrows *)
%token  RIGHT_STAB     (* -> *)
%token  LEFT_STAB      (* <- *)
%token  RIGHT_FAT      (* => *)
%token  LEFT_FAT       (* <= *)
%token  RIGHT_CURVY    (* ~> *)
%token  LEFT_CURVY     (* <~ *)

%token  EXTERN         (* extern command *)
%token <int> INT
%token <int32> INT32
%token <int64> INT64
%token <float> FLOAT
%token <string * string option> STRING
%token <string> TYPE
%token <string> IDENT

(* parens *)
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LCURLY RCURLY
%token SEMICOLON
%token COLON

(* booleans *)
%token TRUE FALSE

(* End of Stream *)
%token EOF

(** 
 * Precedences and associativities.
 *
 * Tokens and rules have precedences.  A reduce/reduce conflict is resolved
 * in favor of the first rule (in source file order).  A shift/reduce conflict
 * is resolved by comparing the precedence and associativity of the token to
 * be shifted with those of the rule to be reduced.
 *
 * By default, a rule has the precedence of its rightmost terminal (if any).
 * 
 * When there is a shift/reduce conflict between a rule and a token that
 * have the same precedence, it is resolved using the associativity:
 * if the token is left-associative, the parser will reduce; if
 * right-associative, the parser will shift; if non-associative,
 * the parser will declare a syntax error.
 * 
 * We will only use associativities with operators of the kind  x * x -> x
 * for example, in the rules of the form    expr: expr BINOP expr
 * in all other cases, we define two precedences if needed to resolve
 * conflicts.
 * 
 * The precedences must be listed from low to high.
 *)

(* %nonassoc IN *)
%nonassoc below_SEMI
(* %left SEMI NEWLINE                  (\* below EQUAL ({lbl=...; lbl=...}) *\) *)
(* %nonassoc LET                           (\* above SEMI ( ...; let ... in ...) *\) *)
(* %nonassoc below_WITH *)
(* %nonassoc FUNCTION WITH                 (\* below BAR  (match ... with ...) *\) *)
(* (\* %nonassoc AND             (\* above WITH (module rec A: SIG with ... and ...) *\) *\) *)
(* %nonassoc ELSE                          (\* (if ...  ... else ...) *\) *)
(* (\* %nonassoc LESSMINUS                     (\* below COLONEQUAL (lbl <- x := e) *\) *\) *)
(* (\*%right    COLONEQUAL                    (\* expr (e := e := e) *\) *\) *)
(* (\*%nonassoc AS *\) *)
(* %left     BAR                           (\* pattern (p|p|p) *\) *)
%nonassoc below_COMMA
(* %left     COMMA                         (\* expr/expr_comma_list (e,e,e) *\) *)
%right RIGHT_STAB                          (* core_type2 (t -> t -> t) *)
(* %left SEMI NEWLINE *)
(* %right    OR                            (\* expr (e || e || e) *\) *)
(* %right    AMPERSAND AND                 (\* expr (e && e && e) *\) *)
(* %nonassoc below_EQUAL *)
(* (\* %left     INFIXOP0 EQUAL LESS GREATER   (\\* expr (e OP e OP e) *\\) *\) *)
(* (\* %right    INFIXOP1                      (\\* expr (e OP e OP e) *\\) *\) *)
(* (\* %right    COLONCOLON                    (\* expr (e :: e :: e) *\) *\) *)
(* %left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT PLUSEQ (\* expr (e OP e OP e) *\) *)
(* %left     PERCENT (\* INFIXOP3 *\) ASTERISK           (\* expr (e OP e OP e) *\) *)
(* (\* %right    INFIXOP4                      (\\* expr (e OP e OP e) *\\) *\) *)
(* %nonassoc prec_unary_minus prec_unary_plus (\* unary - *\) *)
(* %nonassoc prec_constant_constructor     (\* cf. simple_expr (C versus C x) *\) *)
(* %nonassoc prec_constr_appl              (\* above AS BAR COLONCOLON COMMA *\) *)
%nonassoc below_SHARP
(* (\* %nonassoc SHARP                         (\\* simple_expr/toplevel_directive *\\) *\) *)
(* %nonassoc below_DOT *)
(* %nonassoc DOT *)
(* (\* Finally, the first tokens of simple_expr are above everything else. *\) *)
(* %nonassoc BACKQUOTE BANG CHAR FALSE FLOAT INT INT32 INT64 *)
(*           LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN *)
(*           NEW NATIVEINT PREFIXOP STRING TRUE UIDENT *)
(*           LBRACKETPERCENT LBRACKETPERCENTPERCENT *)



(* Entry points into the parser*)
%start <ParseTree.Nodes.Structure.t> file_input
(* %start <Parsetree.core_type> parse_core_type *)
%start <ParseTree.Nodes.Expression.t> parse_expression
%start <ParseTree.Nodes.Pattern.t> parse_pattern
%%

(* Macros *)

%public %inline delimited_by(delim,X): 
  | d = delimited(delim,X,delim) { d }

%public %inline preceded_by(delim,X): 
  | delim x = X  { x }


(* Entry point rules *)

file_input:
  | NEWLINE* s = structure EOF { s }

(* parse_core_type: *)
(*   | c = core_type EOF { c } *)

parse_expression:
  | s = seq_expr EOF { s }

parse_pattern:
  | p = pattern EOF { p }

%inline many_delim(X,delim): x = X delim* { x }

structure: 
  | se = many_delim(structure_item, NEWLINE)+ { se }

structure_item:
  (* TODO: Top-level let here *)
  | FUNCTION i = simple_pattern f = func_proto_body
    { make_structure (StructureItem.Value (ValueBinding.make ~location:(symbol_rloc $startpos $endpos) i f)) $startpos $endpos }
  (*| f = floating_attribute { make_structure(Pstr_attribute f) }*)
  (* | error { expected "a top-level declaration" $startpos $endpos; *)
  (*           make_structure StructureItem.Error $startpos $endpos } *)

sep:
  | NEWLINE { }
  | SEMICOLON { }

(* Core Expressions *)

(* A seq_expr represents a linear execution of sequential
 * expressions (aka procedural execution).
 * This production shouldn't usually be used on it's own,
 * instead using `block`, except in odd cases such as `let`.
 *)
seq_expr: 
  | e = expr %prec below_SEMI { e }
  | e = expr sep              { reloc_exp e $startpos $endpos }
  | e = expr sep s = seq_expr { make_expression (Expression.Sequence (e, s)) $startpos $endpos }
  (* | error { Queue.add (Syntax_error.Expecting ((symbol_rloc $startpos $endpos), "expr")) errors; *)
  (*           make_expression Pexp_err $startpos $endpos } *)


			      
labeled_simple_pattern:
  | i = IDENT s = simple_pattern { (i, None, s) }
  | s = simple_pattern           { ("", None, s) }

(* This rule is for default values. e.g. *)
(* fn foo( bar = baz ) *)
(*             ^~~~~^  *)
(*
opt_default:
  | (* Empty *)         { None }
  | EQUALS s = seq_expr { Some s }
*)

let_pattern:
  | p = pattern { p }
  | p = pattern COLON c = core_type { make_pattern (Pattern.Constraint(p,c)) $startpos $endpos }

				    
expr: 
  | s = simple_expr %prec below_SHARP { s }
  | LPAREN t = expr COMMA tl = separated_nonempty_list(COMMA, expr) RPAREN %prec below_COMMA 
      { make_expression (Expression.Tuple (t::tl)) $startpos $endpos }
  | c = call        { c }
  | a = apply       { a }
  | l = let_main    { l }
  (* TODO: match *)
  (* tuples *)
  | f = anon_func { f }
  (* | error *)
  (*   { expected "an expression" $startpos $endpos; *)
  (*     make_expression Expression.Error $startpos $endpos } *)

call:
  | e = expr arg_list = delimited(LPAREN,separated_list(COMMA,expr),RPAREN)
      { make_expression (Expression.Call (e, arg_list)) $startpos $endpos }
		  
apply: 
  | e = expr arg_list = delimited(LBRACKET,separated_list(COMMA,expr),RBRACKET)
      { make_expression (Expression.Apply (e, arg_list)) $startpos $endpos }

      
simple_expr:
  | LPAREN e = expr RPAREN { e }
  (* | LPAREN e = expr error  *)
  (*   { unclosed "{" $startpos($1) $endpos($1) "}" $startpos($3) $endpos($3); *)
  (*     make_expression Expression.Error $startpos $endpos } *)
  | c = constant 
	  { make_expression (Expression.Constant c) $startpos $endpos }
  | b = block { b }
  | v = IDENT { make_expression (Expression.Ident (mkrhs (Fqident.parse v) $startpos(v) $endpos(v))) $startpos $endpos }


block:
  (* a block is either expr, { seq_expr }, or BEGIN seq_expr END *)
  | LCURLY te = seq_expr RCURLY 
      { reloc_exp te $startpos $endpos }
  (* | LCURLY seq_expr error *)
  (*     { unclosed "{" $startpos($1) $endpos($1) "}" $startpos($3) $endpos($3); *)
  (*       make_expression Pexp_err $startpos $endpos } *)
  | BEGIN te = seq_expr END
      { reloc_exp te $startpos $endpos }
  (* | BEGIN seq_expr error *)
  (*   { unclosed "begin" $startpos($1) $endpos($1) "end" $startpos($3) $endpos($3);  *)
  (*     make_expression Pexp_err $startpos $endpos } *)
  (* | BEGIN e = error { e } *)

(** Patterns *)

pattern:
  | s = simple_pattern { s }
  | s = simple_pattern COLON ct = core_type 
      { make_pattern (Pattern.Constraint (s, ct)) $startpos(s) $endpos(s) }

simple_pattern: 
  | v = IDENT { make_pattern (Pattern.Variable (mkrhs v $startpos(v) $endpos(v))) $startpos $endpos }
  | UNDERSCORE { make_pattern Pattern.Any $startpos $endpos }
  (* | c = constant *)
  | error { expected "a pattern" $startpos $endpos ~suggestion:"use an '_' or '()'";
  	    make_pattern Pattern.Error $startpos $endpos }


(* Core types *)
core_type:
  | s = simple_core_type_or_tuple { s }
  | c1 = core_type RIGHT_STAB c2 = core_type 
      { make_type (Type.Arrow (c1, c2)) $startpos $endpos }

simple_core_type:
  | QUOTE i = IDENT  { make_type (Type.Variable i) $startpos $endpos }
  | UNDERSCORE { make_type  Type.Any    $startpos $endpos }
  | t = TYPE   { make_type (Type.Literal t) $startpos $endpos }

simple_core_type_or_tuple:
  | s = simple_core_type   { s }
  | LPAREN ct = simple_core_type COMMA ctl = separated_nonempty_list(COMMA, simple_core_type) RPAREN 
     { make_type (Type.Tuple (ct::ctl)) $startpos $endpos }

      
anon_func: 
  (* fn [tail] *)
  | FUNCTION f = func_proto_body { f }
  (* |x,y,...z| [tail] *)
  | pl = delimited_by(PIPE, separated_nonempty_list(COMMA,pattern)) e = func_proto_tail
      { make_expression (Expression.Function (pl, e)) $startpos $endpos }
  (* | error func_proto_body  *)
  (*     { make_expression Pexp_err $startpos($1) $endpos($1) } *)

func_proto_body: 
  (* (x...) [tail] *)
  | p = delimited(LPAREN, arg_list, RPAREN) e = func_proto_tail
    { make_expression (Expression.Function (p, e)) $startpos $endpos }
  (* [tail] *)
  | e = func_proto_tail { make_expression (Expression.Function ([],e)) $startpos $endpos }
 
func_proto_tail:
  (* | rs = RIGHT_STAB e = expr sep+ seq_expr en = END { *)
  (*     match e.Expression.variant with  *)
  (*     | Expression.Constant (Const_unit) -> *)
  (* 	 unexpected "function body. Function already returns ()" ~suggestion:"Unit instead of ()" $startpos(rs) $endpos(en); *)
  (* 	 make_expression (Expression.Error) $startpos(e) $endpos(e) *)
  (*     | Expression.Ident f ->  *)
  (* 	 let last : string = Fqident.last (f.Location.txt) in *)
  (* 	 unexpected (Printf.sprintf "function body. Function already returns the value of %s" last) *)
  (* 		    ~suggestion:(Printf.sprintf "a type variable, such as '%s" last) $startpos(rs) $endpos(en); *)
  (* 	 make_expression (Expression.Error) $startpos(e) $endpos(e) *)
  (*   } *)
  
  (* -> [body] *)
  | RIGHT_STAB body = expr { body }
  
  (* -> Unit { [body] } *)
  | RIGHT_STAB ct = core_type sep* LCURLY sep* body = seq_expr RCURLY
    { make_expression (Expression.Constraint (body, ct)) $startpos(body) $endpos(body) }

  (* -> [Type]; [body] end *)
  | RIGHT_STAB ct = core_type sep+ body = seq_expr END
    { make_expression (Expression.Constraint (body, ct)) $startpos(body) $endpos(body) }
 
  (* ; [body] end *)
  | sep+ body = seq_expr END { body }
		
  (* | sep seq_expr error  *) (* To be used for missing END *)

%inline arg_list: sl = separated_list(COMMA, pattern) { sl }

let_main: 
  | LET sep* a = let_binding lt = let_binding* IN sep* e = seq_expr END 
      { make_expression (Expression.Let (a::lt, e)) $startpos $endpos }

let_binding: 
  | lb = let_binding_ sep
     { let (p, e) = lb in ValueBinding.make ~location:(symbol_rloc $startpos $endpos) p e }


let_binding_:
  | p = pattern EQUAL e = expr { (p, e) }
/*  | v = var EQUAL e = expr p = pnt_exec?  { }
  | v = var LEFT_STAB e = expr p = pnt_exec?  { } 
  | v = var p = pnt_exec { } */

pnt_exec: 
  | RIGHT_STAB e = expr { Expression.Pipelined_call(e) }
  | RIGHT_FAT e = expr { Expression.Forked_Call(e) }


(* Constants *)
constant:
  | i = INT         { Const_int i }
  | sd = STRING     { let (s,d) = sd in Const_string (s,d) }
  | f = FLOAT       { Const_float f }
  | i = INT32       { Const_int32 i }
  | i = INT64       { Const_int64 i }
  | LPAREN RPAREN   { Const_unit }
  | TRUE            { Const_true }
  | FALSE           { Const_false }

signed_constant:
  | c = constant        { c }
  | MINUS i = INT       { Const_int(- i) }
  | MINUS f = FLOAT     { Const_float("-" ^ f) }
  | MINUS i = INT32     { Const_int32(Int32.neg i) }
  | MINUS i = INT64     { Const_int64(Int64.neg i) }
  | PLUS i = INT        { Const_int i }
  | PLUS f = FLOAT      { Const_float f }
  | PLUS i = INT32      { Const_int32 i }
  | PLUS i = INT64      { Const_int64 i }
