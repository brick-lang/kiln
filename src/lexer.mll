{
open Parser

let reservedWords = [
  (* Keywords *)
  ("let", Parser.LET);
  ("fn", Parser.FUNCTION);
]

(* Support functions *)

let symbolTable = Hashtbl.create 1024
let _ = List.iter (fun (str,f) -> Hashtbl.add symbolTable str f) reservedWords



(* our white-space counter, stack, and associated functions *)
let ws_count = ref 0
let (ws_stack : int Stack.t) = Stack.create ()
let ws_flag : bool ref = ref false

}



let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let bindigit = ['0' '1']

let int = '-' ? digit+
let frac = '.' digit*
let exp = 'e' ['-' '+']? digit+
let float = digit* frac? exp?

let hexnum = "0x" hexdigit+
let binnum = "0b" bindigit+ 

let newline = '\r' | '\n' | "\r\n"
let ident = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let type_re = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule main = parse
  | "#="     { ws_flag := false; Printf.printf "BLOCK COMMENT BEGIN\n"; block_comment lexbuf }
  | '#'      { ws_flag := false; Printf.printf "LINE COMMENT BEGIN\n"; comment lexbuf }
  | '|'      { ws_flag := false; PIPE }
  | '='      { ws_flag := false; EQUALS }
  | '+'      { ws_flag := false; PLUS }
  | '-'      { ws_flag := false; MINUS }
  | '*'      { ws_flag := false; ASTERISK }
  | '/'      { ws_flag := false; FWD_SLASH }
  | ','      { ws_flag := false; COMMA }
  | '{'      { ws_flag := false; LCURLY }
  | '}'      { ws_flag := false; RCURLY }
  | ':'      { ws_flag := false; COLON }
  | ';'      { ws_flag := false; SEMICOLON }
  | "->"     { ws_flag := false; RIGHT_STAB }
  | "<-"     { ws_flag := false; LEFT_STAB }
  | "=>"     { ws_flag := false; RIGHT_FAT }  
  | "<="     { ws_flag := false; LEFT_FAT }    
  | "~>"     { ws_flag := false; RIGHT_CURVY }    
  | "<~"     { ws_flag := false; LEFT_CURVY }      
  | "<=>"     { ws_flag := false; SPACESHIP } 
  | int as i    { ws_flag := false; INTEGER (int_of_string i) }
  | float as f  { ws_flag := false; FLOAT (float_of_string f) }
  | type_re as t { ws_flag := false; TYPE t }
  | ident as i  { try Hashtbl.find symbolTable i
               with Not_found -> ws_flag := false; IDENT i }
  | eof      { ws_flag := false; EOS }
  | _ { indent lexbuf }
      
and indent = parse
 (* Comment starts *)
  | "#="  { Printf.printf "BLOCK COMMENT BEGIN\n"; block_comment lexbuf }
  | '#'   { Printf.printf "LINE COMMENT BEGIN\n"; comment lexbuf }

 (* Whitespace *)
  | ' '   { incr ws_count; indent lexbuf; }
  | '\t'  { ws_count := !ws_count + 4; indent lexbuf; }
  | newline  { ws_count := 0; ws_flag := true; 
               if !ws_flag then NEWLINE else indent lexbuf}
  | _ { lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1; 
        if !ws_flag then begin
          if !ws_count > Stack.top ws_stack then
            begin
              Stack.push !ws_count ws_stack;
              Parser.INDENT
            end
          else if !ws_count < Stack.top ws_stack then
            begin
              ignore (Stack.pop ws_stack);
              Parser.UNDENT
            end
          else begin
            main lexbuf 
          end
        end
        else begin
          main lexbuf 
        end }
      
and comment = parse
  | newline { ws_count := 0; indent lexbuf}
  | _       { comment lexbuf }

and block_comment = parse
  | "#=" { Printf.printf "BLOCK COMMENT END\n"; main lexbuf }
  | _    { block_comment lexbuf }

