
(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc. 

   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Core
open Format
open Util
open Util.Support.Error
open Frontend

exception Error of string * int * int * string
exception Done

module MI = Parser.MenhirInterpreter

(* type token = Frontend.Parser.token = *)
(*   | VERSION_NUMBER of string *)
(*   | USING *)
(*   | UNDERSCORE *)
(*   | TYPE of string *)
(*   | TRUE *)
(*   | TRAIT *)
(*   | SYNCED *)
(*   | STRING of (string * string option) *)
(*   | SPACESHIP *)
(*   | SEMICOLON *)
(*   | RPAREN *)
(*   | RIGHT_STAB *)
(*   | RIGHT_FAT *)
(*   | RIGHT_CURVY *)
(*   | RETURN *)
(*   | RCURLY *)
(*   | RBRACKET *)
(*   | QUOTE *)
(*   | PLUS *)
(*   | PIPE *)
(*   | PERCENT *)
(*   | PARALLEL *)
(*   | OR *)
(*   | NOT *)
(*   | NEWLINE *)
(*   | MUTATOR *)
(*   | MINUS *)
(*   | METHOD *)
(*   | MATCH *)
(*   | LPAREN *)
(*   | LET *)
(*   | LESS_THAN *)
(*   | LEFT_STAB *)
(*   | LEFT_FAT *)
(*   | LEFT_CURVY *)
(*   | LCURLY *)
(*   | LBRACKET *)
(*   | INT64 of int64 *)
(*   | INT32 of int32 *)
(*   | INT of int *)
(*   | IN *)
(*   | IMPORT *)
(*   | IMPLEMENTS *)
(*   | IF *)
(*   | IDENT of string *)
(*   | GREATER_THAN *)
(*   | FWD_SLASH *)
(*   | FUNCTION *)
(*   | FLOAT of float *)
(*   | FALSE *)
(*   | EXTERN *)
(*   | EQUAL *)
(*   | EOF *)
(*   | END *)
(*   | ELSE *)
(*   | DOUBLE_EQUALS *)
(*   | DOT *)
(*   | COND *)
(*   | COMMA *)
(*   | COLON *)
(*   | CLASS *)
(*   | CHAR of char *)
(*   | BEGIN *)
(*   | BCK_SLASH *)
(*   | BANG *)
(*   | ASTERISK *)
(*   | ASSERT *)
(*   | AND *)
(*   [@@deriving show] *)


(* let show_terminal (type a) (t : a MI.terminal) : string = *)
(*   let open MI in match t with  *)
(*   | T_error -> "T_error" *)
(*   | T_VERSION_NUMBER -> "T_VERSION_NUMBER" *)
(*   | T_USING -> "T_USING" *)
(*   | T_UNDERSCORE -> "T_UNDERSCORE" *)
(*   | T_TYPE -> "T_TYPE" *)
(*   | T_TRUE -> "T_TRUE" *)
(*   | T_TRAIT -> "T_TRAIT" *)
(*   | T_SYNCED -> "T_SYNCED" *)
(*   | T_STRING -> "T_STRING" *)
(*   | T_SPACESHIP -> "T_SPACESHIP" *)
(*   | T_SEMICOLON -> "T_SEMICOLON" *)
(*   | T_RPAREN -> "T_RPAREN" *)
(*   | T_RIGHT_STAB -> "RIGHT_STAB" *)
(*   | T_RIGHT_FAT -> "RIGHT_FAT" *)
(*   | T_RIGHT_CURVY -> "RIGHT_CURVY" *)
(*   | T_RETURN -> "T_RETURN" *)
(*   | T_RCURLY -> "T_RCURLY" *)
(*   | T_RBRACKET -> "T_RBRACKET" *)
(*   | T_QUOTE -> "T_QUOTE" *)
(*   | T_PLUS -> "T_PLUS" *)
(*   | T_PIPE -> "T_PIPE" *)
(*   | T_PERCENT -> "T_PERCENT" *)
(*   | T_PARALLEL -> "T_PARALLEL" *)
(*   | T_OR -> "T_OR" *)
(*   | T_NOT -> "T_NOT" *)
(*   | T_NEWLINE -> "T_NEWLINE" *)
(*   | T_MUTATOR -> "T_MUTATOR" *)
(*   | T_MINUS -> "T_MINUS" *)
(*   | T_METHOD -> "T_METHOD" *)
(*   | T_MATCH -> "T_MATCH" *)
(*   | T_LPAREN -> "T_LPAREN" *)
(*   | T_LET -> "T_LET" *)
(*   | T_LESS_THAN -> "LESS_THAN" *)
(*   | T_LEFT_STAB -> "LEFT_STAB" *)
(*   | T_LEFT_FAT -> "LEFT_FAT" *)
(*   | T_LEFT_CURVY -> "LEFT_CURVY" *)
(*   | T_LCURLY -> "T_LCURLY" *)
(*   | T_LBRACKET -> "T_LBRACKET" *)
(*   | T_INT64 -> "T_INT64" *)
(*   | T_INT32 -> "T_INT32" *)
(*   | T_INT -> "T_INT" *)
(*   | T_IN -> "T_IN" *)
(*   | T_IMPORT -> "T_IMPORT" *)
(*   | T_IMPLEMENTS -> "T_IMPLEMENTS" *)
(*   | T_IF -> "T_IF" *)
(*   | T_IDENT -> "T_IDENT" *)
(*   | T_GREATER_THAN -> "GREATER_THAN" *)
(*   | T_FWD_SLASH -> "FWD_SLASH" *)
(*   | T_FUNCTION -> "T_FUNCTION" *)
(*   | T_FLOAT -> "T_FLOAT" *)
(*   | T_FALSE -> "T_FALSE" *)
(*   | T_EXTERN -> "T_EXTERN" *)
(*   | T_EQUAL -> "T_EQUAL" *)
(*   | T_EOF -> "T_EOF" *)
(*   | T_END -> "T_END" *)
(*   | T_ELSE -> "T_ELSE" *)
(*   | T_DOUBLE_EQUALS -> "DOUBLE_EQUALS" *)
(*   | T_DOT -> "T_DOT" *)
(*   | T_COND -> "T_COND" *)
(*   | T_COMMA -> "T_COMMA" *)
(*   | T_COLON -> "T_COLON" *)
(*   | T_CLASS -> "T_CLASS" *)
(*   | T_BEGIN -> "T_BEGIN" *)
(*   | T_BCK_SLASH -> "BCK_SLASH" *)
(*   | T_BANG -> "T_BANG" *)
(*   | T_ASTERISK -> "T_ASTERISK" *)
(*   | T_ASSERT -> "T_ASSERT" *)
(*   | T_AND -> "T_AND" *)

(* let show_nonterminal (type a) (nt : a MI.nonterminal) : string = *)
(*   let open MI in match nt with *)
(*   | N_structure_item -> "N_structure_item" *)
(*   | N_structure -> "N_structure" *)
(*   | N_simple_pattern -> "N_simple_pattern" *)
(*   | N_simple_expr -> "N_simple_expr" *)
(*   | N_simple_core_type_or_tuple -> "N_simple_core_type_or_tuple" *)
(*   | N_simple_core_type -> "N_simple_core_type" *)
(*   | N_seq_expr -> "N_seq_expr" *)
(*   | N_separated_nonempty_list_COMMA_simple_core_type_ -> "N_separated_nonempty_list_COMMA_simple_core_type_" *)
(*   | N_separated_nonempty_list_COMMA_pattern_ -> "N_separated_nonempty_list_COMMA_pattern_" *)
(*   | N_separated_nonempty_list_COMMA_expr_ -> "N_separated_nonempty_list_COMMA_expr_" *)
(*   | N_sep -> "N_sep" *)
(*   | N_pattern -> "N_pattern" *)
(*   (\* | N_parse_pattern -> "N_parse_pattern" *\) *)
(*   (\* | N_parse_expression -> "N_parse_expression" *\) *)
(*   | N_nonempty_list_sep_ -> "N_nonempty_list_sep_" *)
(*   | N_nonempty_list_many_delim_structure_item_NEWLINE__ -> "N_nonempty_list_many_delim_structure_item_NEWLINE__" *)
(*   | N_loption_separated_nonempty_list_COMMA_pattern__ -> "N_loption_separated_nonempty_list_COMMA_pattern__" *)
(*   | N_loption_separated_nonempty_list_COMMA_expr__ -> "N_loption_separated_nonempty_list_COMMA_expr__" *)
(*   | N_list_sep_ -> "N_list_sep_" *)
(*   | N_list_let_binding_ -> "N_list_let_binding_" *)
(*   (\* | N_separated_nonempty_list_nonempty_list_sep__let_binding_ -> "N_separated_nonempty_list_nonempty_list_sep__let_binding_" *\) *)
(*   | N_list_NEWLINE_ -> "N_list_NEWLINE_" *)
(*   | N_let_main -> "N_let_main" *)
(*   (\* | N_let_binding_ -> "N_let_binding_" *\) *)
(*   | N_let_binding -> "N_let_binding" *)
(*   | N_func_proto_tail -> "N_func_proto_tail" *)
(*   | N_func_proto_body -> "N_func_proto_body" *)
(*   | N_file_input -> "N_file_input" *)
(*   | N_expr -> "N_expr" *)
(*   | N_core_type -> "N_core_type" *)
(*   | N_constant -> "N_constant" *)
(*   | N_call -> "N_call" *)
(*   | N_block -> "N_block" *)
(*   | N_apply -> "N_apply" *)
(*   | N_anon_func -> "N_anon_func" *)

(* let print_symbol (s:MI.xsymbol) =  match s with *)
(*   | MI.X sym -> match sym with *)
(*     | MI.T symbol -> print_string @@ show_terminal symbol *)
(*     | MI.N symbol -> print_string @@ show_nonterminal symbol *)

(* module MIP = MenhirLib.Printers.Make(MI)(struct *)
(*     let print = print_string *)
(*     let print_symbol = print_symbol *)
(*     let print_element = None *)
(*   end) *)

let symbol_rloc start_pos end_pos = {
  Location.loc_start = Sedlexing.pos_sedlexing start_pos;
  Location.loc_end   = Sedlexing.pos_sedlexing end_pos;
  Location.loc_ghost = false;
};;


let unclosed opening_name opening_sp opening_ep closing_name closing_sp closing_ep =
  Parser.Error.prepare_error (ParserError.Unmatched(symbol_rloc opening_sp opening_ep, opening_name,
                                                    symbol_rloc closing_sp closing_ep, closing_name))

let unexpected ?(suggestion="") name opening closing  =
  Parser.Error.prepare_error (ParserError.Not_expecting(symbol_rloc opening closing, name, suggestion))

let expected ?(suggestion="") name opening closing =
  Parser.Error.Expecting(symbol_rloc opening closing, name, suggestion)

module Queue = struct
  include Core.Queue
  let dequeue_apply ~f queue =
    try for i=0 to (Core.Queue.length queue) - 1 do
        let elem = Core.Queue.dequeue_exn queue in
        f elem
      done
    with Caml.Queue.Empty -> ()
end


let get_next_token (lexer : Sedlexing.lexbuf -> Parser.token) =
  let rec next (lexbuf : Sedlexing.lexbuf) =
    try
      let token = lexer lexbuf in
      let startp = Sedlexing.lexeme_start_position lexbuf
      and endp = Sedlexing.lexeme_end_position lexbuf in
      token, startp, endp
    with Lexer.Error ->   (* FIXME: This is really gross *)
      Queue.dequeue_apply Lexer.errors ~f:(fun error ->
          (match !Lexer.last_lexer_error with
           | Some(le) ->
               if Lexer.is_illegal_char_error error &&
                  Lexer.is_illegal_char_error le then ()
               else Location.print_error lexbuf @@ Lexer.prepare_error error;
           | None -> Location.print_error lexbuf @@ Lexer.prepare_error error;
          );
          Lexer.last_lexer_error := Some(error);
        );
      next lexbuf
  in next

let parse lexbuf lexer checkpoint =
  let read_triple = get_next_token lexer in
  let print_errors () =
    if not @@ Queue.is_empty Parser.Error.errors then
      Queue.dequeue_apply Parser.Error.errors
        ~f:(Location.print_error lexbuf *.* Parser.Error.prepare_error);
  in
  let print_error = Location.print_error lexbuf *.* Parser.Error.prepare_error in
  let rec parse_loop lexbuf last_triple (inputneeded, checkpoint) =
    print_errors ();
    match checkpoint with
    | MI.InputNeeded env ->
        (* Update the last recorded [InputNeeded] checkpoint. *)
        let inputneeded = checkpoint in
        let (tok,_,_) as triple = read_triple lexbuf in
        let checkpoint = MI.offer checkpoint triple in
        (* print_endline @@ show_token tok; *)
        parse_loop lexbuf (Some triple) (inputneeded, checkpoint)

    | MI.Shifting _
    | MI.AboutToReduce _ ->
        let checkpoint = MI.resume checkpoint in
        parse_loop lexbuf last_triple (inputneeded, checkpoint)

    | MI.HandlingError env ->
        (* env is *supposed* to have the current token but menhir's interface *)
        (* only defines 'positions' to get the token's positions, not its actual value. /sigh *)
        let (last_tok,startp,endp) = Option.value_exn last_triple in
        let last_env = match inputneeded with MI.InputNeeded e -> e | _ -> assert false in
        (* let handle_error env = *)
        (*   let stack = MI.stack env in *)
        (*   match Lazy.force stack with *)
        (*   | MenhirLib.General.Nil -> () *)
        (*   | MenhirLib.General.Cons (MI.Element (s, v, p1, p2), _) -> begin *)
        (*       match MI.incoming_symbol s with *)
        (*       | MI.T incoming -> () *)
        (*       | MI.N incoming -> begin *)
        (*           match incoming with *)
        (*           | MI.N_simple_pattern -> begin *)
        (*               match last_tok with *)
        (*               | OR -> *)
        (*                   let err = expected "a pattern, but instead found ||" ~suggestion:"remove pipes" startp endp in *)
        (*                   print_error err *)
        (*               | _ -> () *)
        (*             end *)
        (*           | _ -> () *)
        (*         end *)
        (*     end *)
        (* in *)
        (* let get_curr_value (el:MI.element) : ParseTree = *)
        (*   match el with *)
        (*   | MI.Element (x,v,_,_) -> v *)
        (* in *)

        (* print_endline @@ show_token last_tok; *)
        (* handle_error env; *)

        (* print_endline "CURRENT ENV:"; *)
        (* MIP.print_env env; *)

        (* print_newline (); *)
        (* print_endline "LAST GOOD ENV:"; *)
        (* MIP.print_env last_env; *)
        (* print_newline (); *)


        (* TODO: Do fancy error-recovery things here *)
        let checkpoint = MI.resume checkpoint in
        parse_loop lexbuf last_triple (inputneeded, checkpoint)

    | MI.Accepted v -> Some v

    | MI.Rejected ->
        print_endline "ERROR";
        print_errors (); None

  in
  assert (match checkpoint with MI.InputNeeded _ -> true | _ -> false);
  parse_loop lexbuf None (checkpoint, checkpoint)


let parse_file inFile =
  let pi = In_channel.create inFile in
  let lexbuf = Sedlexing.Utf8.from_channel pi in
  let pos : Lexing.position = { Lexing.pos_fname = inFile;
                                Lexing.pos_lnum = 0; Lexing.pos_bol = 0; Lexing.pos_cnum = 0} in
  let file_parser = Parser.Incremental.file_input pos in
  Location.init lexbuf inFile;
  let result =  parse lexbuf Lexer.main file_parser in
  Parsing.clear_parser ();
  In_channel.close pi;
  if !Lexer.lexing_error then None else result

let codegen_file inFile = 
  let parsed = match parse_file inFile with Some s -> s | _ -> failwith "Malformed parsetree." in
  let codegened = List.hd_exn @@ List.map parsed ~f:(Codegen.codegen_structure_item) in
  let module_name = inFile |> Filename.basename |> Filename.chop_extension in
  Codegen.dump_module ()

let codegen_files l = List.map ~f:codegen_file l; ()

(* let process_file f  = *)
(*   let cmds = parse_file f in *)
(*   List.iter cmds ~f:(fun e -> Llvm.dump_value (Codegen.codegen_expr e)) *)
