%{
open Ast
open Lexing

let parse_error s =
  begin
    try
      let start_pos = Parsing.symbol_start_pos ()
      and end_pos = Parsing.symbol_end_pos () in
      Printf.printf "File \"%s\", line %d, characters %d-%d: \n"
          start_pos.pos_fname
          start_pos.pos_lnum
          (start_pos.pos_cnum - start_pos.pos_bol)
          (end_pos.pos_cnum - start_pos.pos_bol)
    with Invalid_argument(_) -> ()
  end;
  Printf.printf "Syntax error: %s\n" s;
  raise Parsing.Parse_error
%}

/* keyword */
%token  LET  /* variable binding*/
%token  FUNCTION /* function creation */

/* ops */
%token  PIPE           /* | */
%token  COMMA          /* , */
%token  EQUALS         /* = */
%token  DOUBLE_EQUALS  /* == */
%token  SPACESHIP      /* <=> */
%token  BANG           /* ! */
%token  PLUS
%token MINUS
%token ASTERISK
%token FWD_SLASH
%token BCK_SLASH


%token INDENT UNDENT
%token NEWLINE

/* arrows */
%token  RIGHT_STAB     /* -> */
%token  LEFT_STAB      /* <- */
%token  RIGHT_FAT      /* => */
%token  LEFT_FAT       /* <= */
%token  RIGHT_CURVY    /* ~> */
%token  LEFT_CURVY     /* <~ */

%token  EXTERN         /* extern command */
%token <int> INTEGER
%token <float> FLOAT
%token <string> STRING
%token <string> TYPE
%token <string> IDENT

/* parens */
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LCURLY RCURLY
%token SEMICOLON
%token COLON

/* booleans */
%token TRUE FALSE

/* Unit */
%token UNIT

/* End of Stream */
%token EOS

%start <Ast.toplevel> stmt
%%

stmt: 
  | l = lambda { TopFun(l) }

expr:
  | i = INTEGER { let (i : expr) = Integer(i) in i }
  | f = FLOAT   { let (f : expr) = Float(f) in f }
  | i = IDENT   { let (i : expr) = Ident(i) in i }
  | l = lambda  { Lambda(l) }

lambda: 
  | vb = var_bind; sep_or_e; bl = block { let (vb : variable list) = vb in Function(Prototype("", vb), bl) }

var_bind: 
  | PIPE; a = arg_list; PIPE { let (a : variable list) = a in a }
/*  | (* Empty *) { [ ] }*/

arg_list:
  | a = arg_list ; COMMA; v = var { a @ [v] }
  | v = var { [v] }
  | (* Empty *) { [ ] }

let_main:
  | LET; a = assignment; INDENT; lt = let_tail; e = expr 
    { Let(a::lt, e) }

let_tail : 
  | (* empty *) { [ ] }
  | lt = let_tail; a = assignment { lt @ [a] }


assignment: 

/*  |PIPE Var EQUALS Exp Pnt_or_E */

  | PIPE; v = var; EQUALS; e = expr; NEWLINE { Binding(v, e, No_Call) }

/*  | PIPE Var LEFT_STAB Exp Pnt_or_E 
  | PIPE Var Pnt_Exec

*/

pnt_or_e: 
  | (* Empty *) { No_Call } 
  | p = pnt_exec { p } ;

pnt_exec: 
  | RIGHT_STAB; e = expr { Pipelined_Call(e) }
  | RIGHT_FAT; e = expr { Forked_Call(e) }

var: 
  | BANG; i = IDENT; t = type_annot { Ref_Var(i, t) }
  | i = IDENT; t = type_annot { Plain_Var(i, t) }
   

type_annot: 
  | (* Empty *) { Unknown_Type }
  | COLON; t = TYPE { Known_Type(t) }

term_expr: 
  | (* empty *) { [ ] }
  | te = term_expr; e = expr; sep { te@[e] }

sep_or_e:
  | (* Empty *) { }
  | sep_or_e sep { }


block: 
  /* a block is either expr, { expr }, or INDENT expr UNDENT */
  | LCURLY; te = term_expr; RCURLY { Block(te) }
  | INDENT; te = term_expr; UNDENT { Block(te) }
  | e = expr { e } 

sep:
  | NEWLINE { }
  | SEMICOLON { }


/*
expr:
  | v = value { v }
  | LPAREN; e = expr; RPAREN      { e }
  | LPAREN expr              { parse_error "expected ')'" }

toplevel:
  | s = statement; terminator { s }
  | t = terminator { t }
  ;

statement:
  | e = expr        { Expression (Function (Prototype ("", [||]), e)) }
  | e = extern      { Extern e }
  | d = definition  { Definition d }

terminator:
  | EOS       { End }
  | v = value { Some v }
  ;


value:
  | s = STRING  { String s }
  | i = INT     { Int i }
  | f = FLOAT   { Float f }
  | TRUE        { Bool true }
  | FALSE       { Bool false }
  ;


object_fields: 
  | obj = rev_object_fields { List.rev obj }
  ;

rev_object_fields:
  | (* empty *) { [] }
  | obj = rev_object_fields; COMMA; k = ID; COLON; v = value
    { (k, v) :: obj }
  ;


array_values:
  | (* empty *) { [] }
  | vl = rev_values { List.rev vl }
  ;

rev_values:
  | v = value { [v] }
  | vl = rev_values; COMMA; v = value
    { v :: vl }
  ;
*/