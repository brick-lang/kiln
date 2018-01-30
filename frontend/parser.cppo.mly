%{
open Lexing
open ParseTree
module Error = ParserError

#define here     (Common.Location.rloc $startpos    $endpos)
#define there(v) (Common.Location.rloc $startpos(v) $endpos(v))

let mkrhs ~loc rhs = Common.Location.mkloc rhs loc
let reloc_pat x sp ep = { x with Pattern.location = Common.Location.rloc sp ep }
let reloc_exp x sp ep = { x with Expression.location = Common.Location.rloc sp ep }

let make_structureexp e = {
  StructureItem.variant = StructureItem.Value e;
  StructureItem.location = e.ValueBinding.location
}

%}

(* TOKENS *)
%token  BEGIN
%token  AS	
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
%token  USING
%token  WITHIN
%token  IMPLEMENTS
%token  IMPORT
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
%token <string> STRING
%token <char> CHAR
%token <string> TYPE
%token <string> IDENT
%token <string> VERSION_NUMBER

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

%nonassoc IN
%nonassoc below_SEMI
%left SEMICOLON NEWLINE                  (* below EQUAL ({lbl=...; lbl=...}) *)
%nonassoc LET                           (* above SEMI ( ...; let ... in ...) *)
(* %nonassoc below_WITH *)
(* %nonassoc FUNCTION WITH                 (\* below BAR  (match ... with ...) *\) *)
(* (\* %nonassoc AND             (\* above WITH (module rec A: SIG with ... and ...) *\) *\) *)
%nonassoc ELSE                          (* (if ...  ... else ...) *)
(* (\* %nonassoc LESSMINUS                     (\* below COLONEQUAL (lbl <- x := e) *\) *\) *)
(* (\*%right    COLONEQUAL                    (\* expr (e := e := e) *\) *\) *)
(* (\*%nonassoc AS *\) *)
(* %left     BAR                           (\* pattern (p|p|p) *\) *)
%nonassoc below_COMMA
(* %left     COMMA                         (\* expr/expr_comma_list (e,e,e) *\) *)
%left COLON				   (* expr : core_type *)
%right RIGHT_STAB                          (* core_type2 (t -> t -> t) *)
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
%nonassoc BACKQUOTE BANG CHAR FALSE FLOAT INT INT32 INT64
	  LBRACE LBRACKET LPAREN STRING TRUE


%type <ParseTree.Structure.t option> option(preceded(nonempty_list(sep),structure_toplevel))
%type <ParseTree.Structure.t> structure_toplevel
%type <ParseTree.StructureItem.t> structure_item_toplevel structure_item
%type <unit list> nonempty_list(sep) list(sep)
%type <unit> sep

%type <ParseTree.Expression.t option> option(preceded(EQUAL,expr))
%type <ParseTree.Expression.t list> separated_nonempty_list(COMMA,expr) loption(separated_nonempty_list(COMMA,expr))
%type <ParseTree.Expression.t> seq_expr expr call apply simple_expr block

%type <ParseTree.Pattern.t list> separated_nonempty_list(COMMA,simple_pattern)
%type <ParseTree.Pattern.t> labeled_simple_pattern pattern simple_pattern

%type <ParseTree.Type.t list> separated_nonempty_list(COMMA,simple_core_type)  separated_nonempty_list(COMMA,core_type)
%type <ParseTree.Type.t> core_type simple_core_type simple_core_type_or_tuple

%type <ParseTree.Expression.t> anon_func func_proto_body func_proto_tail

%type <ParseTree.PatternDefault.t list> loption(separated_nonempty_list(COMMA,pattern_opt_default))
%type <ParseTree.PatternDefault.t list> separated_nonempty_list(COMMA,pattern_opt_default)
%type <ParseTree.PatternDefault.t> pattern_opt_default

%type <ParseTree.Expression.t> let_main

%type <ParseTree.LetStatement.t list> let_binding_many
%type <ParseTree.LetStatement.t> let_binding

%type <ParseTree.Expression.variant> pnt_exec

%type <ParseTree.Constant.t> constant signed_constant

(* Entry points into the parser*)
%start <ParseTree.Structure.t> file_input
(* %start <Parsetree.core_type> parse_core_type *)
(* %start <ParseTree.Nodes.Expression.t> parse_expression *)
(* %start <ParseTree.Nodes.Pattern.t> parse_pattern *)
%%

(* Macros *)

%public %inline delimited_by(delim,X):
  | d = delimited(delim,X,delim) { d }

(* Entry point rules *)

file_input:
  | sep* s = structure_toplevel EOF { s }

(* parse_core_type: *)
(*   | c = core_type EOF { c } *)

(* parse_expression: *)
(*   | s = seq_expr EOF { s } *)

(* parse_pattern: *)
(*   | p = pattern EOF { p } *)

(* Toplevel structure *)
structure_toplevel:
  | (* ε *) { [] }
  | se = structure_item_toplevel st = preceded(sep+, structure_toplevel)? 
      { match st with Some st -> se::st | None -> [se] }

(* Toplevel structure items *)
structure_item_toplevel:

  (* using Mortar[0.0.1] *)
  | USING t = TYPE LBRACKET v = VERSION_NUMBER RBRACKET
      { StructureItem.using (Type.lit t ~loc:there(t)) v ~loc:here }

  | WITHIN t = TYPE
      { StructureItem.within (Type.lit t ~loc:there(t)) ~loc:here }
  
  (* structure... *)
  | s = structure_item { s }


(* Structure items *)
(* Usable at the toplevel and inside [Modules] *)
structure_item:

  (* [pattern] = [expr] *)
  | i = pattern EQUAL e = expr
     { StructureItem.value ~loc:here (ValueBinding.make i e ~loc:here) }

  (* fn [name] [body]  *)
  | FUNCTION i = simple_pattern f = func_proto_body
      { StructureItem.value ~loc:here (ValueBinding.make i f ~loc:here) }

  (* import [Module] *)
  | IMPORT t = TYPE
      { StructureItem.import ~loc:here (Type.lit t ~loc:here) }

  (* module Something; [stuct_item]; end *)
  (* | MODULE t = TYPE sep+ se = many_delim(structure_item, sep)* END { se } *)

  (*| f = floating_attribute { make_structure(Pstr_attribute f) }*)
  (* | error { expected "a top-level declaration" $startpos $endpos; *)
  (*           make_structure StructureItem.Error $startpos $endpos } *)


(* Separators *)
sep:
  | NEWLINE { }
  | SEMICOLON { }


(************************************************************
 ************************************************************)

(* Core Expressions *)

(* A seq_expr represents a linear execution of sequential
 * expressions (aka procedural execution).
 * This production shouldn't usually be used on it's own,
 * instead using `block`, except in odd cases such as `let`.
 *)
seq_expr:
  (* [expr] *)
  | e = expr %prec below_SEMI { e }

  (* [expr] [sep]+ *)
  | e = expr sep+              { reloc_exp e $startpos $endpos }

  (* [expr] [sep]+ [seq_expr] *)
  | e = expr sep+ s = seq_expr { Expression.sequence e s ~loc:here }
  (* | error { Queue.add (Syntax_error.Expecting ((rloc $startpos $endpos), "expr")) errors; *)
  (*           make_expression Pexp_err $startpos $endpos } *)



labeled_simple_pattern:
  (* [ident] [pattern] *)
  | i = IDENT s = simple_pattern { (i, None, s) }

  (* [pattern] *)
  | s = simple_pattern           { ("", None, s) }

expr:
  | s = simple_expr %prec below_SHARP { s }
  | LPAREN t = expr COMMA tl = separated_nonempty_list(COMMA, expr) RPAREN %prec below_COMMA
      { Expression.tuple ~loc:here (t::tl) }
  | LBRACKET t = expr COMMA tl = separated_nonempty_list(COMMA, expr) RBRACKET %prec below_COMMA
      { Expression.vector ~loc:here (t::tl) }
  | c = call        { c }
  | a = apply       { a }
  | l = let_main    { l }
  (* TODO: match *)
  (* tuples *)
  | f = anon_func { f }
  (* | c = constructor { c } *)

call:
  (* expr(a1,a2,...) *)
  | e = expr arg_list = delimited(LPAREN,separated_list(COMMA,expr),RPAREN)
      { Expression.call ~loc:here e arg_list }

apply:
  (* expr[a1,a2,...] *)
  | e = expr arg_list = delimited(LBRACKET,separated_list(COMMA,expr),RBRACKET)
      { Expression.apply ~loc:here e arg_list }


simple_expr: 
 | LPAREN e = expr RPAREN { e }
 | c = constant { Expression.constant c ~loc:here }
 | b = block { b }
 | v = IDENT { Expression.ident (Fqident.parse v) ~loc:here }


block:
  (* a block is either expr, { seq_expr }, or BEGIN seq_expr END *)
  | LCURLY te = seq_expr RCURLY
      { reloc_exp te $startpos $endpos }
  | BEGIN te = seq_expr END
      { reloc_exp te $startpos $endpos }

(** Patterns *)
pattern:
  | s = simple_pattern { s }
  | s = simple_pattern COLON ct = core_type
      { Pattern.constraint_ ~loc:here s ct }

simple_pattern:
  | v = IDENT { Pattern.var v ~loc:here }
  | UNDERSCORE { Pattern.any () ~loc:here }
  | p = pattern AS v = IDENT { Pattern.alias p v ~loc:here }

  (* (elem1,elem2,_) *)
  | LPAREN ct = simple_pattern COMMA ctl = separated_nonempty_list(COMMA, simple_pattern) RPAREN
      { Pattern.tuple (ct::ctl) ~loc:here }

  (* [e1,e2,_] *)
  | LBRACKET ct = simple_pattern COMMA ctl = separated_nonempty_list(COMMA, simple_pattern) RBRACKET
      { Pattern.vector (ct::ctl) ~loc:here }

  (* TODO: [e1,e2,es...] *)
  
  (* elem *)
  | c = constant { Pattern.constant c ~loc:here }


(* Core types *)
core_type:
  | s = simple_core_type_or_tuple { s }
  | c1 = core_type RIGHT_STAB c2 = core_type
      { Type.arrow c1 c2 ~loc:here }

simple_core_type:
  (* Int32 *)
  | t = TYPE 
      { Type.lit t ~loc:here }

  (* 'a *)
  | QUOTE i = IDENT
      { Type.var i ~loc:here }

  (* _ *)
  | UNDERSCORE 
      { Type.any () ~loc:here }

  (* Map<String,Int> *)
  | t = TYPE LESS_THAN ts = separated_nonempty_list(COMMA, core_type) GREATER_THAN
      { Type.constructor (Fqident.parse t) ts ~loc:here }


simple_core_type_or_tuple:
  | s = simple_core_type   { s }

  (* (Int,Int,Float) *)
  | LPAREN ct = simple_core_type COMMA ctl = separated_nonempty_list(COMMA, simple_core_type) RPAREN
     { Type.tuple (ct::ctl) ~loc:here }


anon_func:
  (* fn [tail] *)
  | FUNCTION f = func_proto_body { f }

  (* |x,y,...z| [tail] *)
  | pl = delimited_by(PIPE, separated_nonempty_list(COMMA, pattern_opt_default)) e = func_proto_tail
      { Expression.fn pl e ~loc:here }


func_proto_body:
  (* (x,y...) [tail] *)
  | p = delimited(LPAREN, arg_list, RPAREN) e = func_proto_tail
      { Expression.fn p e ~loc:here }

  (* [tail] *)
  | e = func_proto_tail { Expression.fn [] e ~loc:here }

func_proto_tail:
  (* | rs = RIGHT_STAB e = expr sep+ seq_expr en = END { *)
  (*     match e.Expression.variant with  *)
  (*     | Expression.Constant (Const_unit) -> *)
  (*	 unexpected "function body. Function already returns ()" ~suggestion:"Unit instead of ()" $startpos(rs) $endpos(en); *)
  (*	 make_expression (Expression.Error) $startpos(e) $endpos(e) *)
  (*     | Expression.Ident f ->  *)
  (*	 let last : string = Fqident.last (f.Location.txt) in *)
  (*	 unexpected (Printf.sprintf "function body. Function already returns the value of %s" last) *)
  (*		    ~suggestion:(Printf.sprintf "a type variable, such as '%s" last) $startpos(rs) $endpos(en); *)
  (*	 make_expression (Expression.Error) $startpos(e) $endpos(e) *)
  (*   } *)

  (* -> [body] *)
  | RIGHT_STAB body = expr { body }

  (* -> Unit { [body] } *)
  | RIGHT_STAB ct = core_type LCURLY sep* body = seq_expr RCURLY
    { Expression.constraint_ body ct ~loc:there(body) }

  (* -> [Type]; [body] end *)
  | RIGHT_STAB ct = core_type sep+ body = seq_expr END
    { Expression.constraint_ body ct ~loc:there(body) }

  (* ; [body] end *)
  | sep+ body = seq_expr END { body }

%inline arg_list: sl = separated_list(COMMA, pattern_opt_default) { sl }

(* This rule is for default values. e.g. *)
(* fn foo( bar = baz ) *)
(*         ^--------^  *)
pattern_opt_default:
  | p = pattern e = preceded(EQUAL, expr)? 
      { match e with 
	| None   -> PatternDefault.none p ~loc:here
	| Some d -> PatternDefault.default p d ~loc:here }


let_main:
  (* let ;* [binding] in; [seq_expr] end *)
  | LET sep* lb = let_binding_many sep* IN sep* e = seq_expr END
    { Expression.let_ lb e ~loc:here }

let_binding_many:
  | lb = let_binding { [lb] }
  | lbs = let_binding_many sep+ lb = let_binding { lbs@[lb] }

let_binding:
  (* x = [expr] *)
  | p = pattern EQUAL e = expr
      { LetStatement.binding ~loc:here (ValueBinding.make p e ~loc:here) }

  (* x -> foo(y) *)
  | p = pattern RIGHT_STAB e = expr
      { LetStatement.call ~loc:here (BoundCall.pipelined p e ~loc:here) }

  (* x => foo(y) *)
  | p = pattern RIGHT_FAT e = expr
      { LetStatement.call ~loc:here (BoundCall.forked p e ~loc:here) }

  (* x ~> foo(y) *)
  | p = pattern RIGHT_CURVY e = expr
      { LetStatement.call ~loc:here (BoundCall.synced p e ~loc:here) }

  (* x <- foo(y) *)
  | p = pattern LEFT_STAB e = expr
      { LetStatement.future ~loc:here (FutureBinding.make p e ~loc:here) }

  (* import Mortar.List *)
  | IMPORT t = TYPE
      { LetStatement.import ~loc:here (Type.lit t ~loc:there(t)) }

pnt_exec:
  (* -> [expr] *)
  | RIGHT_STAB e = expr { Expression.Pipelined_call e }
  (* => [expr] *)
  | RIGHT_FAT e = expr { Expression.Forked_Call e }


(* Constants *)
constant:
  | i = INT         { Constant.int i     ~loc:here }
  | s = STRING      { Constant.string s  ~loc:here }
  | f = FLOAT       { Constant.float f   ~loc:here }
  | i = INT32       { Constant.int32 i   ~loc:here }
  | i = INT64       { Constant.int64 i   ~loc:here }
  | LPAREN RPAREN   { Constant.unit ()   ~loc:here }
  | TRUE            { Constant.true_ ()  ~loc:here }
  | FALSE           { Constant.false_ () ~loc:here }

signed_constant:
  | c = constant        { c }
  | MINUS i = INT       { Constant.int(- i) ~loc:here }
  | MINUS f = FLOAT     { Constant.float("-" ^ f)      ~loc:here }
  | MINUS i = INT32     { Constant.int32(Int32.neg i)  ~loc:here }
  | MINUS i = INT64     { Constant.int64(Int64.neg i)  ~loc:here }
  | PLUS i = INT        { Constant.int i    ~loc:here }
  | PLUS f = FLOAT      { Constant.float f  ~loc:here }
  | PLUS i = INT32      { Constant.int32 i  ~loc:here }
  | PLUS i = INT64      { Constant.int64 i  ~loc:here }
