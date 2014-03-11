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

%token  LET  /* variable binding*/
%token  FUNCTION /* function creation */
%token  RETURN

/* ops */
%token  PIPE           /* | */
%token  COMMA          /* , */
%token  EQUALS         /* = */
%token  LESS_THAN
%token  GREATER_THAN
%token  DOUBLE_EQUALS  /* == */
%token  SPACESHIP      /* <=> */
%token  BANG           /* ! */
%token  PLUS
%token  MINUS
%token  ASTERISK
%token  FWD_SLASH
%token  BCK_SLASH


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
%token <bool> BOOL

/* parens */
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LCURLY RCURLY
%token SEMICOLON
%token COLON

/* booleans */
%token TRUE FALSE

/* End of Stream */
%token EOF

%start <Ast.expr list> file_input
%%

%public %inline delimited_by(delim,X): 
  d = delimited(delim,X,delim) { d }

%public %inline preceded_by(delim,X): 
  delim x = X  { x }

file_input:
  | s = compound_stmt EOF { s }

compound_stmt:
  | (* Empty *) { [ ] }
  | NEWLINE n = compound_stmt { n }
  | s = stmt n = compound_stmt  { s::n }

stmt: 
  | e = expr{ e }

expr:
  | i = INTEGER { Integer(i, Some("Integer")) }
  | f = FLOAT   { Float(f, Some("Float"))   }
  | i = IDENT   { Ident(i)   }
  | b = BOOL    { Bool(b, Some("Bool")) }
  | f = func    { f }

func: 
  | FUNCTION p = delimited(LPAREN, arg_list, RPAREN) sep* bl = block 
    { Function("", p, bl) }
  | FUNCTION name = IDENT p = delimited(LPAREN, arg_list, RPAREN) sep* bl = block 
    { Function(name, p, bl) }
  | FUNCTION name = IDENT sep* bl = block 
    { Function(name, [], bl) }
  | vb = delimited_by(PIPE, arg_list) sep* bl = block 
    { Function("", vb, bl) }

%inline arg_list: sl = separated_list(COMMA, var) { sl }

let_main: 
  | LET a = assignment INDENT lt = assignment* e = expr { Let(a::lt, e) }

assignment: 
  | PIPE v = var EQUALS e = expr NEWLINE* { Binding(v, e, No_Call) }
/*  | PIPE v = var EQUALS e = expr p = pnt_exec? NEWLINE* { }
  | PIPE v = var LEFT_STAB e = expr p = pnt_exec? NEWLINE* { } 
  | PIPE v = var p = pnt_exec NEWLINE* { } */

pnt_exec: 
  | RIGHT_STAB e = expr { Pipelined_Call(e) }
  | RIGHT_FAT e = expr { Forked_Call(e) }

var: 
  | BANG i = IDENT t = type_annot { Ref_Var(i, t) }
  | i = IDENT t = type_annot { Plain_Var(i, t) }
   

type_annot:
  | (* Empty *) { None }
  |  p = preceded_by(COLON, TYPE) { Some(p) }


%inline term_expr: e = expr sep* { e }

block: 
  /* a block is either expr, { expr }, or INDENT expr UNDENT */
  | LCURLY te = term_expr* RCURLY { Block(te, None) }
  | INDENT te = term_expr* UNDENT { Block(te, None) }
  | e = expr { e } 

sep:
  | NEWLINE { }
  | SEMICOLON { }

comparison: s = separated_nonempty_list(comp_op, expr) { s }

comp_op:
  | LESS_THAN { }
  | GREATER_THAN { }
  | DOUBLE_EQUALS { }
/*
expr:
  | v = value { v }
  | LPAREN; e = expr; RPAREN      { e }
  | LPAREN expr              { parse_error "expected ')'" }

toplevel:
  | s = statement terminator { s }
  | t = terminator { t }


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
*/
