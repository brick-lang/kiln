(* open Lexer

let main () =
	let lexbuf = Lexing.from_channel stdin in
	Stack.push 0 ws_stack;
	indent lexbuf

let _ = Printexc.print main () *)


(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc. 
   
   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Lexer
open Format
open Support.Error
open Ast
open Core.Std

exception Error of string * int * int * string

let searchpath = ref [""]

let argDefs = [
  "-I",
      Arg.String (fun f -> searchpath := f::!searchpath),
      "Append a directory to the search path"]

let parseArgs () =
  let inFile = ref (None : string option) in
  Arg.parse argDefs
     (fun s ->
       match !inFile with
       | Some(_) -> err "You must specify exactly one input file"
       | None -> inFile := Some(s))
     "";
  match !inFile with
      None -> err "You must specify an input file"
    | Some(s) -> s

let openfile infile = 
  let rec trynext l = match l with
      | [] -> err ("Could not find " ^ infile)
      | (d::rest) -> 
          let name = if d = "" then infile else (d ^ "/" ^ infile) in
          try open_in name
            with Sys_error m -> trynext rest
  in trynext !searchpath

let parseFile inFile =
  let pi = openfile inFile in
  let lexbuf = Lexing.from_channel pi in
  let result =
   Parser.file_input Lexer.indent lexbuf 
  in
  Parsing.clear_parser (); In_channel.close pi; result

let alreadyImported = ref ([] : string list)
  
let process_file f  =
  alreadyImported := f :: !alreadyImported;
  let cmds = parseFile f in
  print_string (string_of_file_input cmds)

let main () = 
  let inFile = parseArgs() in
  Stack.push ws_stack 0;
  let _ = process_file inFile  in
  ()

let () = set_max_boxes 1000
let () = set_margin 67
let () = print_flush()
let () = Exn.handle_uncaught ~exit:true main