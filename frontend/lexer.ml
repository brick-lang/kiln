(* Parts of this file were taken from the OCaml compiler *)
(* Which is licensed under the Q Public License version 1.0 *)

(* The lexer definition *)

open Parser
open Core
module Location = Common.Location

type error =
  | Illegal_character of string * Location.t
  | Illegal_escape of string * Location.t
  | Unterminated_comment of Location.t
  | Unterminated_string of Location.t
  | Unterminated_string_in_comment of Location.t * Location.t
  | Keyword_as_label of string * Location.t
  | Literal_overflow of string * Location.t

let prepare_error err =
  let error = Location.error ~header:"Lexical error" in
  let errorf = Location.errorf ~header:"Lexical error" in
  match err with
  | Illegal_character (c,location) ->
      errorf ~location "Illegal character \"%s\"." c
  | Illegal_escape (s,location) ->
      errorf ~location "Illegal backslash escape in string or character \"%s\"." s
  | Unterminated_comment (location) ->
      error ~location "Comment is unterminated."
  | Unterminated_string location ->
      error ~location "String literal is unterminated."
  | Unterminated_string_in_comment (_, location) ->
      (* let msg = fun () ->  *)
      (*    print_string "This comment contains an unterminated string literal" *)
      (* in *)
      (* Location.print_error ~msg:msg lexbuf loc *)
      error ~location ""
  | Keyword_as_label (kwd,location) ->
      errorf ~location "\"%s\" is a keyword, it cannot be used as label name" kwd
  | Literal_overflow (ty,location) ->
      errorf  ~location "Integer literal exceeds the range of representable \
                         integers of type %s" ty

(* Keyword table *)
let keyword_table = String.Table.create () ~size:1024 ~growth_allowed:false
let _ =
  List.iter ~f:(fun (str,f) -> 
      ignore (String.Table.add keyword_table ~key:str ~data:f)) 
    [
      "and", AND;
      "as", AS;
      "assert", ASSERT;
      "begin", BEGIN;
      "class", CLASS;
      "cond", COND;
      "else", ELSE;
      "end", END;
      "false", FALSE;
      "fn", FUNCTION;
      "if", IF;
      "implements", IMPLEMENTS;
      "import", IMPORT;
      "in", IN;
      "let", LET;
      "match", MATCH;
      "method", METHOD;
      "mutator", MUTATOR;
      "or", OR;
      "parallel", PARALLEL;
      "return", RETURN;
      "synced", SYNCED;
      "trait", TRAIT;
      "true", TRUE;
      "using", USING;
      "within", WITHIN;
    ]


(* To store the position of the beginning of a string and comment *)
let comment_start_loc : Location.t list ref = ref [];;
let in_comment () = !comment_start_loc <> [];;
let print_warnings = ref true

(* To convert integer literals, allowing max_int + 1 (OCaml PR#4210) *)
let cvt_int_literal = int_of_string_opt
let cvt_int32_literal = Caml.Int32.of_string_opt
let cvt_int64_literal = Caml.Int64.of_string_opt
let cvt_nativeint_literal = Caml.Nativeint.of_string_opt

let rcom_count = ref 0

(* Error report *)
let digit    = [%re? '0'..'9']
let hexdigit = [%re? '0'..'9','a'..'f','A'..'F']
let bindigit = [%re? '0'|'1'] 

let int_literal = [%re? Opt '-', Plus digit] (* '-'? digit+ *)
let frac = [%re? '.', (Star digit)] (* '.' digit* *)
let exp = [%re? 'e', Opt ('-'|'+'), Plus digit] (*  'e' ['-' '+']?] digit+ *)
let float_literal = [%re? Star digit, Opt frac, Opt exp]           (* digit* frac? exp? *)

let hexnum = [%re? "0x", Plus hexdigit]     (* "0x" hexdigit+ *)

let binnum = [%re? "0b", Plus bindigit]     (* "0b" bindigit+ *)

let version_number = [%re? Plus digit, '.', Plus digit, '.', Plus digit]

let newline = [%re? '\r' | '\n' | "\r\n"]

let string_literal = [%re? '\"', Star any, '\"']
let unterminated_string_literal = [%re? '\"', Star any]

let char_literal = [%re? '\'', any, '\'']

let ident = [%re? lowercase, Star(alphabetic | '0'..'9' | '_' )]
let fn_ident = [%re? ident, Opt('\''|'?'|'!')] (* function names can end in ' (prime) ? (predicate/bool) or ! (mutator) *)

let type_re = [%re? uppercase, Star(alphabetic | '0'..'9' | '_')]

(* ( Type_re \.)* ident *)
let fqident = [%re? Star(type_re, '.'), fn_ident]

let fqtype = [%re? Star(type_re, '.'), type_re]


let rec main lexbuf : (token,error) Result.t =
  let open Result in
  let lexeme = Sedlexing.Utf8.lexeme in
  match%sedlex lexbuf with
  | newline  -> return NEWLINE
  | " "      -> main lexbuf
  | "#{"     -> incr rcom_count; recursive_comment lexbuf
  | "#="     -> block_comment lexbuf
  | '#'      -> comment lexbuf 
  | '|'      -> return PIPE 
  | '_'      -> return UNDERSCORE 
  | '='      -> return EQUAL 
  | '+'      -> return PLUS 
  | '-'      -> return MINUS 
  | '*'      -> return ASTERISK 
  | '/'      -> return FWD_SLASH 
  | '\\'     -> return BCK_SLASH 
  | ','      -> return COMMA
  | '\''     -> return QUOTE
  | '{'      -> return LCURLY 
  | '}'      -> return RCURLY 
  | '('      -> return LPAREN 
  | ')'      -> return RPAREN 
  | '['      -> return LBRACKET 
  | ']'      -> return RBRACKET 
  | ':'      -> return COLON 
  | ';'      -> return SEMICOLON 
  | '>'      -> return GREATER_THAN
  | '<'      -> return LESS_THAN
  | "->"     -> return RIGHT_STAB 
  | "<-"     -> return LEFT_STAB 
  | "=>"     -> return RIGHT_FAT 
  (* | "<="     -> LEFT_FAT  *)
  | "~>"     -> return RIGHT_CURVY 
  | "<~"     -> return LEFT_CURVY 
  | "<=>"    -> return SPACESHIP 
  | "||"     -> return OR 
  | "&&"     -> return AND 
  | "not"    -> return NOT 

  | char_literal ->
      let c = lexeme lexbuf in
      if String.length c = 3 then
        return @@ CHAR (String.get c 1)
      else
        fail @@ Illegal_character(c, Location.curr lexbuf)

  | int_literal ->
      let i = lexeme lexbuf in
      Result.of_option Option.(cvt_int_literal i >>| fun i -> INT i)
        ~error:(Literal_overflow("int", Location.curr lexbuf))

  | float_literal ->
      Result.of_option Option.(float_of_string_opt @@ lexeme lexbuf >>| fun f -> FLOAT f)
        ~error:(Literal_overflow("float", Location.curr lexbuf))

  | int_literal, "i32" ->
      let i = lexeme lexbuf in
      let i = String.sub i 0 ((String.length i) - 3) in (* remove the 'i32' *)
      Result.of_option Option.(cvt_int32_literal i >>| fun i -> INT32 i)
        ~error:(Literal_overflow("int32", Location.curr lexbuf))

  | int_literal, "i64" ->
      let i = lexeme lexbuf in
      let i = String.sub i 0 ((String.length i) - 3) in
      Result.of_option Option.(cvt_int64_literal i >>| fun i -> INT64 i)
        ~error:(Literal_overflow("int64", Location.curr lexbuf))

  | string_literal ->
      let s = lexeme lexbuf in
      return @@ STRING (String.slice s 1 (String.length s - 1)) (* remove quotes *)

  | unterminated_string_literal ->
      fail (Unterminated_string(Location.curr lexbuf))

  | version_number ->
      return @@ VERSION_NUMBER (lexeme lexbuf)

  | fqtype ->
      return @@ TYPE (lexeme lexbuf)

  | fqident ->
      let i = lexeme lexbuf in begin
	match Hashtbl.find keyword_table i with
	| Some(tok) -> return tok
	| None ->  return @@ IDENT i
      end

  | eof -> return EOF 

  | _ ->
      let old_lexbuf = lexbuf in begin 
        Pervasives.ignore @@ Sedlexing.next lexbuf;
        fail @@ Illegal_character(Sedlexing.Utf8.lexeme old_lexbuf, Location.curr old_lexbuf)
      end

and comment lexbuf = match%sedlex lexbuf with
  (* We consume the token here, because newlines
   * at the end of a comment are not considered part of the
   * token stream, and then resume lexing the next line
   * as source *)
  | newline -> main lexbuf

  | eof -> Result.return EOF

  (* Otherwise, we're part of the comment *)
  | _ -> ignore @@ Sedlexing.next lexbuf; comment lexbuf 

and block_comment lexbuf = match%sedlex lexbuf with
  | "#=" -> main lexbuf
  | eof  -> Result.return EOF
  | _    -> ignore @@ Sedlexing.next lexbuf; block_comment lexbuf


(* Support a nested comment syntax! (* Like OCaml! *) *)
and recursive_comment lexbuf = match%sedlex lexbuf with
  | "#{" -> incr rcom_count; recursive_comment lexbuf
  | "}#" -> decr rcom_count;
      if !rcom_count > 0
      then recursive_comment lexbuf
      else main lexbuf

  | newline -> recursive_comment lexbuf
  | eof  -> Result.return EOF
  | _    -> ignore @@ Sedlexing.next lexbuf; recursive_comment lexbuf
