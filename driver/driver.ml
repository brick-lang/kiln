
(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc. 

   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Core
open Format
open Common
open Util.Basic
open Frontend

type error =
  | LexerError of Lexer.error
  | ParserError of Parser.Error.error
  | Rejected

module State = struct
  type t = {
    errors: error list;
    warnings: unit list;
    (* warnings: warning Queue.t; *)
  }
  let empty = {errors=[]; warnings=[]}
end

module StateM = struct
  module Let_syntax = Util.StateMonad.Make(State)
  include Let_syntax

  let error e : unit t = fun s -> State.{s with errors = e::s.errors }, ()
  let warn w : unit t = fun s -> State.{s with warnings = w::s.warnings }, ()
end 
module MI = Parser.MenhirInterpreter

let rec get_next_token lexer lexbuf = let open Result.Let_syntax in
  let%bind token = lexer lexbuf in
  let startp = Sedlexing.lexeme_start_position lexbuf in
  let endp = Sedlexing.lexeme_end_position lexbuf in
  return (token, startp, endp)

let handle_error (lexbuf:Sedlexing.lexbuf) last_triple env (inputneeded, checkpoint) =
  let open StateM in 
  let elem = MI.top env in
  match elem with
  | None -> return (inputneeded, checkpoint)
  | Some (MI.Element(s, v, p1, p2)) -> begin match MI.incoming_symbol s with
      | MI.T incoming -> return (inputneeded, checkpoint)
      | MI.N incoming -> begin match incoming with
          | MI.N_simple_pattern -> begin match last_triple with
              | Some(Parser.OR,startp,endp) ->
                  error @@ ParserError (Parser.Error.unexpected "OR" ~suggestion:"a pattern" startp endp) >>
                  let cp = MI.offer inputneeded (Parser.FUNCTION, p1, p2) in
                  return (cp,cp)
              | _ -> return (inputneeded, checkpoint)
            end
          | _ -> return (inputneeded, checkpoint)
        end
    end

let parse (lexbuf:Sedlexing.lexbuf) lexer checkpoint =
  let read_triple = get_next_token lexer in
  let rec parse_loop lexbuf last_triple (inputneeded, checkpoint) =
    let open StateM in
    match checkpoint with
    | MI.InputNeeded env -> begin
        (* Update the last recorded [InputNeeded] checkpoint. *)
        match read_triple lexbuf with
        | Ok t ->
            let inputneeded = checkpoint in
            let checkpoint = MI.offer checkpoint t in
            parse_loop lexbuf (Some t) (inputneeded, checkpoint)

        | Error err ->
            error @@ LexerError err >>
            parse_loop lexbuf last_triple (inputneeded, checkpoint)
      end

    | MI.Shifting _
    | MI.AboutToReduce _ ->
        let checkpoint = MI.resume checkpoint in
        parse_loop lexbuf last_triple (inputneeded, checkpoint)

    | MI.HandlingError env ->
        let%bind (inputneeded, checkpoint) = handle_error lexbuf last_triple env (inputneeded, checkpoint) in
        let checkpoint = MI.resume checkpoint in
        parse_loop lexbuf last_triple (inputneeded, checkpoint)

    | MI.Accepted(v:ParseTree.Structure.t) -> StateM.return @@ Some v
    | MI.Rejected -> error Rejected >> return None
  in
  assert (match checkpoint with MI.InputNeeded _ -> true | _ -> false);
  parse_loop lexbuf None (checkpoint, checkpoint)


let parse_file inFile =
  let pos = Lexing.{ pos_fname = inFile;
                     pos_lnum = 0; pos_bol = 0; pos_cnum = 0} in
  let parse_channel chan =
    let parser = Parser.Incremental.file_input pos in
    let lexbuf = Location.init inFile @@ Sedlexing.Utf8.from_channel chan in
    let (state, result) = StateM.run (parse lexbuf Lexer.main parser) State.empty in
    if List.length state.errors > 0 then
      let errors = List.rev state.errors in
      let _ = List.iter errors ~f:(function
          | ParserError e -> Location.print_error chan @@ Parser.Error.prepare_error e
          | LexerError e  -> Location.print_error chan @@ Lexer.prepare_error e
          | Rejected -> ())
      in
      None
    else
      result
  in 
  In_channel.with_file inFile ~f:parse_channel

let codegen_file inFile = 
  let parsed = match parse_file inFile with Some s -> s | _ -> failwith "Malformed parsetree." in
  let codegened = List.hd_exn @@ List.map parsed ~f:(Codegen.codegen_structure_item) in
  let module_name = inFile |> Filename.basename |> Filename.chop_extension in
  Codegen.dump_module ()

let codegen_files l = List.map ~f:codegen_file l; ()

(* let process_file f  = *)
(*   let cmds = parse_file f in *)
(*   List.iter cmds ~f:(fun e -> Llvm.dump_value (Codegen.codegen_expr e)) *)
