(* Parts of this file were taken from the OCaml compiler *)
(* Which is licensed under the Q Public License version 1.0 *)

(* The lexer definition *)

open Parser
open Core

type error =
  | Illegal_character of string * Location.t
  | Illegal_escape of string * Location.t
  | Unterminated_comment of Location.t
  | Unterminated_string of Location.t
  | Unterminated_string_in_comment of Location.t * Location.t
  | Keyword_as_label of string * Location.t
  | Literal_overflow of string * Location.t
;;

exception Error
let errors : error Queue.t = Queue.create ();;
let lexing_error = ref false;;
let last_lexer_error : error option ref = ref None;;
let is_illegal_char_error = (function Illegal_character(_,_) -> true | _ -> false);;

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

let handle_error lexbuf error =
  Queue.enqueue errors error;
  lexing_error := true


(* Keyword table *)

let keyword_table = String.Table.create () ~size:1024 ~growth_allowed:false
let _ =  List.iter ~f:(fun (str,f) -> 
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
      "if", IF;
      "import", IMPORT;
      "in", IN;
      "match", MATCH;
      "or", OR;
      "let", LET;
      "trait", TRAIT;
      "method", METHOD;
      "mutator", MUTATOR;
      "synced", SYNCED;
      "parallel", PARALLEL;
      "fn", FUNCTION;
      "return", RETURN;
      "true", TRUE;
      "false", FALSE;
      "using", USING;
      "implements", IMPLEMENTS;
    ]


(* To buffer string literals *)

let initial_string_buffer = Caml.Bytes.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= Caml.Bytes.length !string_buff then begin
    let new_buff = Caml.Bytes.create @@ Caml.Bytes.length (!string_buff) * 2 in
    Caml.Bytes.blit !string_buff 0 new_buff 0 @@ Caml.Bytes.length !string_buff;
    string_buff := new_buff
  end;
  ignore (Caml.Bytes.unsafe_set !string_buff !string_index c);
  incr string_index

let store_string s =
  for i = 0 to String.length s - 1 do
    store_string_char s.[i];
  done

let store_lexeme lexbuf =
  store_string @@ Lexing.lexeme lexbuf

let get_stored_string () =
  let s = Caml.Bytes.sub_string !string_buff 0 !string_index in
  string_buff := initial_string_buffer;
  s

(* To store the position of the beginning of a string and comment *)
let string_start_loc = ref Location.none;;
let comment_start_loc : Location.t list ref = ref [];;
let in_comment () = !comment_start_loc <> [];;
let is_in_string = ref false
let in_string () = !is_in_string
let print_warnings = ref true

(* To translate escape sequences *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * ((Sedlexing.lexeme_char lexbuf i) - 48) +
          10 * ((Sedlexing.lexeme_char lexbuf (i+1)) - 48) +
          ((Sedlexing.lexeme_char lexbuf (i+2)) - 48) in
  if (c < 0 || c > 255) then
    if in_comment () then 'x'
    else begin
      handle_error lexbuf (Illegal_escape (Sedlexing.Utf8.lexeme lexbuf, Location.curr lexbuf));
      raise Error
    end
  else Char.of_int_exn c

let char_for_hexadecimal_code lexbuf i =
  let d1 = Char.to_int (Lexing.lexeme_char lexbuf i) in
  let val1 =
    if d1 >= 97 then d1 - 87
    else if d1 >= 65 then d1 - 55
    else d1 - 48
  in
  let d2 = Char.to_int (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 =
    if d2 >= 97 then d2 - 87
    else if d2 >= 65 then d2 - 55
    else d2 - 48
  in
  Char.of_int_exn (val1 * 16 + val2)

(* To convert integer literals, allowing max_int + 1 (OCaml PR#4210) *)

let cvt_int_literal = int_of_string
let cvt_int32_literal = Int32.of_string
let cvt_int64_literal = Int64.of_string
let cvt_nativeint_literal = Nativeint.of_string

(* let preprocessor = ref None *)


let rcom_count = ref 0

exception Unexpected_token

(* Error report *)

(* let () = Location.register_error_of_exn (function *)
(*       | Error (err, loc) -> *)
(*         Some (Location.error_of_printer loc report_error err) *)
(*       | _ -> None) *)

let extract_option = function
  | Some(i) -> i
  | None -> assert false


let digit    = [%re? '0'..'9']
let hexdigit = [%re? '0'..'9','a'..'f','A'..'F']
let bindigit = [%re? '0'|'1'] 

let int_literal = [%re? Opt '-', Plus digit] (* '-'? digit+ *)
let frac = [%re? '.', (Star digit)] (* '.' digit* *)
let exp = [%re? 'e', Opt ('-'|'+'), Plus digit] (*  'e' ['-' '+']?] digit+ *)
(* let float_literal = [%re? Star digit, Opt frac, Opt exp]           (\* digit* frac? exp? *\) *)

let hexnum = [%re? "0x", Plus hexdigit]     (* "0x" hexdigit+ *)

let binnum = [%re? "0b", Plus bindigit]     (* "0b" bindigit+ *)

let version_number = [%re? Plus digit, '.', Plus digit, '.', Plus digit]

let newline = [%re? '\r' | '\n' | "\r\n"]

let string_literal = [%re? '\"', Star any, '\"']

let char_literal = [%re? '\'', any, '\'']

(* ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']* *)
let ident = [%re? 'a'..'z', Star('a'..'z'|'A'..'Z'|'0'..'9'|'_'|'\'')]

(* ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* *)
let type_re = [%re? 'A'..'Z', Star('a'..'z'|'A'..'Z'|'0'..'9'|'_')]

(* ( Type_re \.)* ident *)
let fqident = [%re? Star(type_re, '.'), ident]

let fqtype = [%re? Star(type_re, '.'), type_re]


let rec main lexbuf =
  match%sedlex lexbuf with
  | newline  -> NEWLINE
  | " "      -> main lexbuf
  | "#{"     -> incr rcom_count; recursive_comment lexbuf
  | "#="     -> block_comment lexbuf
  | '#'      -> comment lexbuf 
  | '|'      -> PIPE 
  | '_'      -> UNDERSCORE 
  | '='      -> EQUAL 
  | '+'      -> PLUS 
  | '-'      -> MINUS 
  | '*'      -> ASTERISK 
  | '/'      -> FWD_SLASH 
  | '\\'     -> BCK_SLASH 
  | ','      -> COMMA
  | '\''     -> QUOTE
  | '{'      -> LCURLY 
  | '}'      -> RCURLY 
  | '('      -> LPAREN 
  | ')'      -> RPAREN 
  | '['      -> LBRACKET 
  | ']'      -> RBRACKET 
  | ':'      -> COLON 
  | ';'      -> SEMICOLON 
  | '>'      -> GREATER_THAN
  | '<'      -> LESS_THAN
  | "->"     -> RIGHT_STAB 
  | "<-"     -> LEFT_STAB 
  | "=>"     -> RIGHT_FAT 
  (* | "<="     -> LEFT_FAT  *)
  | "~>"     -> RIGHT_CURVY 
  | "<~"     -> LEFT_CURVY 
  | "<=>"    -> SPACESHIP 
  | "||"     -> OR 
  | "&&"     -> AND 
  | "not"    -> NOT 

  | char_literal -> let c = Sedlexing.Utf8.lexeme lexbuf in
      assert (String.length c = 3);
      CHAR (String.get c 1)

  | int_literal -> let i = Sedlexing.Utf8.lexeme lexbuf in begin
      try INT (cvt_int_literal i)
      with Failure _ ->
        handle_error lexbuf (Literal_overflow ("int", Location.curr lexbuf));
        raise Error
    end

  (* | float_literal -> let f = Sedlexing.Utf8.lexeme lexbuf in FLOAT f *)

  | int_literal, "i32" -> let i = Sedlexing.Utf8.lexeme lexbuf in begin
      let i = String.sub i 0 ((String.length i) - 3) in
      try INT32 (cvt_int32_literal i)
      with Failure _ ->
        handle_error lexbuf (Literal_overflow ("int32", Location.curr lexbuf));
        raise Error
    end


  | int_literal, "i64" -> let i = Sedlexing.Utf8.lexeme lexbuf in begin
      let i = String.sub i 0 ((String.length i) - 3) in
      try INT64 (cvt_int64_literal i)
      with Failure _ ->
        handle_error lexbuf (Literal_overflow("int64", Location.curr lexbuf));
        raise Error
    end

  | version_number -> let v = Sedlexing.Utf8.lexeme lexbuf in VERSION_NUMBER v

  | fqtype -> let t = Sedlexing.Utf8.lexeme lexbuf in TYPE t 

  | fqident -> let i = Sedlexing.Utf8.lexeme lexbuf in begin
      match Hashtbl.find keyword_table i with
      | Some(tok) -> tok
      | None ->  IDENT i
    end

  | eof -> EOF 

  | _ ->
      let old_lexbuf = lexbuf in
      ignore @@ Sedlexing.next lexbuf;
      handle_error old_lexbuf (Illegal_character(Sedlexing.Utf8.lexeme old_lexbuf, Location.curr old_lexbuf));
      raise Error

and comment lexbuf = match%sedlex lexbuf with
  (* We consume the token here, because newlines
   * at the end of a comment are not considered part of the
   * token stream, and then resume lexing the next line
   * as source *)
  | newline -> main lexbuf

  | eof -> EOF

  (* Otherwise, we're part of the comment *)
  | _ -> ignore @@ Sedlexing.next lexbuf; comment lexbuf 

and block_comment lexbuf = match%sedlex lexbuf with
  | "#=" -> main lexbuf
  | eof  -> EOF
  | _    -> ignore @@ Sedlexing.next lexbuf; block_comment lexbuf


(* Support a nested comment syntax! (* Like OCaml! *) *)
and recursive_comment lexbuf = match%sedlex lexbuf with
  | "#{" -> incr rcom_count; recursive_comment lexbuf
  | "}#" -> decr rcom_count;
      if !rcom_count > 0
      then recursive_comment lexbuf
      else main lexbuf

  | newline -> recursive_comment lexbuf
  | eof  -> EOF
  | _    -> ignore @@ Sedlexing.next lexbuf; recursive_comment lexbuf
