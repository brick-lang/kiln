
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
  let rec parse_loop lexbuf read_token checkpoint =
    match checkpoint with
    | MI.InputNeeded env ->
        if not @@ Queue.is_empty Parser.Error.errors then
          Core.Std.Queue.dequeue_apply Parser.Error.errors
            ~f:(fun e -> Location.print_error lexbuf (Parser.Error.prepare_error e));
        let token = read_token lexbuf in (* Get a new token from the lexer *)
        let checkpoint = MI.offer checkpoint token in
        parse_loop lexbuf read_token checkpoint
    | MI.Shifting _
    | MI.AboutToReduce _ ->
       let checkpoint = MI.resume checkpoint in
       parse_loop lexbuf read_token checkpoint
    | MI.HandlingError env ->
       (* The parser has suspended itself because of a syntax error. Stop. *)
        parse_loop lexbuf read_token (MI.resume checkpoint)
    | MI.Accepted v -> Some v
    | MI.Rejected -> None
  in parse_loop lexbuf (get_next_token lexer) parser

let parse_file inFile =
  let pi = open_in inFile in
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
  Ollvm.Ez.Module.init module_name ("x86_64", "pc", "linux-gnu") "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
  |> Codegen.ModuleMonad.exec codegened


(* let process_file f  = *)
(*   let cmds = parse_file f in *)
(*   List.iter cmds ~f:(fun e -> Llvm.dump_value (Codegen.codegen_expr e)) *)

