%{
  // Bring the standard library into the
  // global namespace
  using namespace std;

  // Prototypes to keep the compiler happy
  void yyerror (const char *error);
  int  yylex ();
%}

/*** yacc/bison Declarations ***/

/* Require bison 3.0 or later */
%require "3.0"

/* add debug output code to generated parser. disable this for release
 * versions. */
%debug

/* start symbol is named "start" */
//%start start

/* write out a header file containing the token defines */
%defines

%locations

/* use newer C++ skeleton file */
%skeleton "lalr1.cc"

/* namespace to enclose parser in */
%define api.namespace {kiln}

/* set the parser's class identifier */
%define parser_class_name {Parser}

/* set the lexer's class identifier */
%name-prefix "kiln::Lexer::"

/* verbose error messages */
%error-verbose


%token INDENT UNDENT
%token NUMBER
%token PLUS

%%

expression : NUMBER PLUS NUMBER
           ;

%%
