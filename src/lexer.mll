{
open Parser
open Core.Std

let reservedWords = [
  (* Keywords *)
  ("let", Parser.LET);
  ("fn", Parser.FUNCTION);
  ("return", Parser.RETURN);
  ("true", (Parser.BOOL true));
  ("false", (Parser.BOOL false));
]

(* Support functions *)

let symbolTable = String.Table.create () ~size:1024 ~growth_allowed:false
let _ = List.iter ~f:(fun (str,f) -> ignore (String.Table.add symbolTable ~key:str ~data:f)) reservedWords


(* our white-space counter, stack, and associated functions *)
let ws_count : int ref = ref 0
let ws_stack : int Stack.t = Stack.create ()
let ws_flag : bool ref = ref false

let () = Stack.push ws_stack 0           (* Prime the lexer *)

exception Unexpected_token

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
  | "#="     { ws_flag := false; block_comment lexbuf }
  | '#'      { ws_flag := false; comment lexbuf }
  | '|'      { ws_flag := false; PIPE }
  | '='      { ws_flag := false; EQUALS }
  | '+'      { ws_flag := false; PLUS }
  | '-'      { ws_flag := false; MINUS }
  | '*'      { ws_flag := false; ASTERISK }
  | '/'      { ws_flag := false; FWD_SLASH }
  | ','      { ws_flag := false; COMMA }
  | '{'      { ws_flag := false; LCURLY }
  | '}'      { ws_flag := false; RCURLY }
  | '('      { ws_flag := false; LPAREN }
  | ')'      { ws_flag := false; RPAREN }
  | '['      { ws_flag := false; LBRACKET }
  | ']'      { ws_flag := false; RBRACKET }  
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
  | float as f  { ws_flag := false; FLOAT (Float.of_string f) }
  | type_re as t { ws_flag := false; TYPE t }
  | ident as i  { ws_flag := false;   
                  match Hashtbl.find symbolTable i with
                  | Some(tok) -> tok
                  | None ->  IDENT i }
  | eof      { ws_flag := false; EOF }
  | newline { lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1;  indent lexbuf}
  | _ { raise Unexpected_token }
      
and indent = parse
 (* Comment starts *)
  | "#="  { block_comment lexbuf }
  | '#'   { comment lexbuf }

 (* Whitespace *)
  | ' '   { incr ws_count; indent lexbuf; }
  | '\t'  { ws_count := !ws_count + 4; indent lexbuf; }
  | newline  { Lexing.new_line lexbuf; ws_count := 0; ws_flag := true; 
               if !ws_flag then NEWLINE else indent lexbuf}
  | eof { lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1;
          if !ws_flag then begin
          let top = match Stack.top ws_stack with Some(v) -> v | None -> assert false in
            if !ws_count > top then
              begin
                Stack.push ws_stack !ws_count ;
                Parser.INDENT
              end
            else if !ws_count < top then
              begin
                ignore (Stack.pop ws_stack);
                Parser.UNDENT
              end
            else EOF (*assert false *)
          end
          else EOF }
  | _ { lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1; 
        if !ws_flag then begin
          let top = match Stack.top ws_stack with Some(v) -> v | None -> assert false in        
          if !ws_count > top then
            begin
              Stack.push  ws_stack !ws_count;
              Parser.INDENT
            end
          else if !ws_count < top then
            begin
              ignore (Stack.pop ws_stack);
              Parser.UNDENT
            end
          else main lexbuf
        end
        else main lexbuf
      }
      
and comment = parse
  | newline { ws_count := 0; ws_flag := true; indent lexbuf}
  | _       { comment lexbuf }

and block_comment = parse
  | "#=" { main lexbuf }
  | _    { block_comment lexbuf }

