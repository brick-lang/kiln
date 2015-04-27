
(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc. 

   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Core.Std
open Format
open Util.Support.Error
open Frontend

exception Error of string * int * int * string
exception Done

module Core = struct
  module Std = struct
    module Queue = struct
      include Core.Std.Queue
      let dequeue_apply ~f queue =
        try for i=0 to (Core.Std.Queue.length queue) - 1 do
            let elem = Core.Std.Queue.dequeue_exn queue in
            f elem
          done
        with Caml.Queue.Empty -> ()
    end
  end
end

let get_next_token (lexer : Sedlexing.lexbuf -> Parser.token) =
  let rec next (lexbuf : Sedlexing.lexbuf) =
    try
      let token = lexer lexbuf in
      let startp = Sedlexing.lexeme_start_position lexbuf
      and endp = Sedlexing.lexeme_end_position lexbuf in
      token, startp, endp
    with Lexer.Error ->   (* FIXME: This is really gross *)
      Core.Std.Queue.dequeue_apply Lexer.errors ~f:(fun error ->
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

module MI = Parser.MenhirInterpreter

let parse lexbuf lexer parser =
  let rec parse_loop lexbuf read_token result needs_error_handling =
    match result with
    | MI.InputNeeded env ->
        if needs_error_handling then
          Core.Std.Queue.dequeue_apply Parser.Error.errors
            ~f:(fun e -> Location.print_error lexbuf (Parser.Error.prepare_error e));
        let token = read_token lexbuf in (* Get a new token from the lexer *)
        let result = MI.offer env token in
        parse_loop lexbuf read_token result false
    | MI.HandlingError env ->
        parse_loop lexbuf read_token (MI.handle env) true
    | MI.Accepted v -> Some v
    | MI.Rejected -> None
  in parse_loop lexbuf (get_next_token lexer) parser false

let parse_file inFile =
  let pi = open_in inFile in
  let lexbuf = Sedlexing.Utf8.from_channel pi in
  let file_parser = Parser.file_input_incremental () in
  Location.init lexbuf inFile;
  let result =  parse lexbuf Lexer.main file_parser in
  Parsing.clear_parser ();
  In_channel.close pi;
  if !Lexer.lexing_error then None else result

(* let process_file f  = *)
(*   let cmds = parse_file f in *)
(*   List.iter cmds ~f:(fun e -> Llvm.dump_value (Codegen.codegen_expr e)) *)

