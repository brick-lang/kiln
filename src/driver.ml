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

open Format
open Support.Error
open Ast
open Printer
open Core.Std

exception Error of string * int * int * string

let parseFile inFile =
  let pi = open_in inFile in
  let lexbuf = Lexing.from_channel pi in
  let result = Parser.file_input Lexer.indent lexbuf in
  Parsing.clear_parser (); 
  In_channel.close pi; 
  result

let process_file f  =
  let cmds = parseFile f in
  print_string (string_of_file_input cmds)
