exception Error

type token = 
  | UNDENT
  | TYPE of (string)
  | TRUE
  | STRING of (string)
  | SPACESHIP
  | SEMICOLON
  | RPAREN
  | RIGHT_STAB
  | RIGHT_FAT
  | RIGHT_CURVY
  | RCURLY
  | RBRACKET
  | PLUS
  | PIPE
  | NEWLINE
  | MINUS
  | LPAREN
  | LET
  | LESS_THAN
  | LEFT_STAB
  | LEFT_FAT
  | LEFT_CURVY
  | LCURLY
  | LBRACKET
  | INTEGER of (int)
  | INDENT
  | IDENT of (string)
  | GREATER_THAN
  | FWD_SLASH
  | FUNCTION
  | FLOAT of (float)
  | FALSE
  | EXTERN
  | EQUALS
  | EOF
  | DOUBLE_EQUALS
  | COMMA
  | COLON
  | BCK_SLASH
  | BANG
  | ASTERISK


val file_input: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr list)