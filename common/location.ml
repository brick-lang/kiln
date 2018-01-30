
(* Originally from the OCaml compiler, which is licensed under the QPL.
 * This has been adapted to use Sedlexing, and will be heavily modified
 * in the future. *)

open Sedlexing
open Core
open Util.Basic

let pp_position fmt p =
  Format.fprintf fmt "{ file_name = %s;\n" p.file_name;
  Format.fprintf fmt "    buffer_offset = %d;\n" p.buffer_offset;
  Format.fprintf fmt "    line_number = %d;\n" p.line_number;
  Format.fprintf fmt "    line_offset = %d }" p.line_offset;

type t = {
  loc_start: position;
  loc_end: position;
  loc_ghost: bool
} [@@deriving show]

let rloc start_pos end_pos = {
  loc_start = Sedlexing.pos_sedlexing start_pos;
  loc_end   = Sedlexing.pos_sedlexing end_pos;
  loc_ghost = false;
}

let gloc start_pos end_pos = {
  loc_start = Sedlexing.pos_sedlexing start_pos;
  loc_end   = Sedlexing.pos_sedlexing end_pos;
  loc_ghost = true;
}

let in_file name =
  let loc = {
    file_name = name;
    line_number = 1;
    line_offset = 0;
    buffer_offset = -1;
  } in
  { loc_start = loc; loc_end = loc; loc_ghost = true }


let none = in_file "_none_";;

let curr lexbuf = {
  loc_start = lexbuf.start_pos;
  loc_end = lexbuf.curr_pos;
  loc_ghost = false
}

let init fname lexbuf =
  lexbuf.curr_pos <- {
    file_name = fname;
    line_number = 1;
    line_offset = 0;
    buffer_offset = 0;
  };
  lexbuf

let input_name = ref "_none_"

(* return file, line, char from the given position *)
let get_pos_info pos =
  (pos.file_name, pos.line_number, pos.buffer_offset - pos.line_offset + 1)

open Textutils

let highlight_textutils (header: unit -> unit ) chan loc =
  let start_offset = loc.loc_start.line_offset in

  (* Determine line numbers for the start and end points *)
  let start_line = loc.loc_start.line_number in
  let end_line = loc.loc_end.line_number in

  (* Lines of input to work with *)
  let lines =
    In_channel.seek chan (Int64.of_int start_offset);
    List.init (end_line - start_line + 1) ~f:id
    |> List.map ~f:(fun _ -> In_channel.input_line_exn chan)
  in

  (* Buffer.output_buffer stdout segment; *)

  (** Underlines a location  *)
  let underliner start stop =
    if start = stop then
      Console.Ansi.printf [`Green] "^"
    else
      let stop = stop - 1 in
      for _i = start to stop do
        if _i = start || _i = stop then
          Console.Ansi.printf [`Green] "^"
        else
          Console.Ansi.printf [`Green] "~"
      done
  in

  (* Code is indented two spaces *)
  let indent c = Out_channel.output_string c "  " in

  let curr_line = ref start_line in
  let pos_at_bol = ref loc.loc_start.line_offset in

  (** Prints a single character, depending on its position *)
  let handler line =
    let open Out_channel in
    newline stdout;
    indent stdout;
    print_string line;

    let start_idx = loc.loc_start.buffer_offset in
    let end_idx = loc.loc_end.buffer_offset in

    (* Hit the end of the last line; time to underline *)
    if !curr_line = end_line then begin
      newline stdout;
      indent stdout;
      print_string @@ String.make (start_idx - !pos_at_bol) ' ';
      underliner start_idx end_idx;
    end;

    incr curr_line;
    pos_at_bol := !pos_at_bol + (String.length line)
  in
  (* Print character location (useful for Emacs) *)
  let fname, line, char = get_pos_info loc.loc_start in
  Console.Ansi.printf [`Bright] "%s:%d:%d" fname line char;
  header ();
  List.iter lines ~f:handler;
  Out_channel.newline stdout;
  Out_channel.flush stdout

open Format

let absolute_path s = (* This function could go into Filename *)
  let open Filename in
  let s = if is_relative s then concat (Sys.getcwd ()) s else s in
  (* Now simplify . and .. components *)
  let rec aux s =
    let base = basename s in
    let dir = dirname s in
    if dir = s then dir
    else if base = current_dir_name then aux dir
    else if base = parent_dir_name then dirname (aux dir)
    else concat (aux dir) base
  in
  aux s

let show_filename ?(absolute=false) file =
  if absolute then absolute_path file else file

let print_filename ppf file =
  Format.fprintf ppf "%s" (show_filename file)

let (msg_file, msg_line, msg_chars, msg_to, msg_colon) =
  ("File \"", "\", line ", ", characters ", "-", ":")

(* let print_error_cur_file ppf = print_error ppf (in_file !input_name);; *)

let print_warning lb loc w =
  (* if Warnings.is_active w then begin *)
  (* let printw ppf w = *)
  (*   let n = Warnings.print w in *)
  (*   num_loc_lines := !num_loc_lines + n *)
  (* in *)
  (* print lb loc; *)
  (* fprintf ppf "Warning %a@." printw w; *)
  (* pp_print_flush ppf (); *)
  (* incr num_loc_lines; *)
  (* end *)
  assert false

(* let prerr_warning loc w = print_warning loc err_formatter w;; *)

type 'a loc = {
  txt : 'a;
  loc : t;
}

let mkloc txt loc = { txt ; loc }
let mknoloc txt = mkloc txt none

type error = {
  location: t;
  header:string;
  message: string option;
  sub_errors: error list;
}

let rec print_error chan ?(sub_error=false) { header; message; location; sub_errors} =
  let header_helper () : unit = 
    Console.Ansi.printf [`Red; `Bright] " Error: ";
    Console.Ansi.printf [`Bright] "%s" header;
    ()
  in
  if not sub_error then
    highlight_textutils header_helper chan location;
  (match message with
   | Some m -> 
       if not sub_error then
         Console.Ansi.printf [`Bright] "Reason: "
       else
         Console.Ansi.printf [`Bright] "Suggestion: ";
       print_endline m
   | None -> ());
  ignore @@ List.map ~f:(print_error chan ~sub_error:true) sub_errors;
  if List.length sub_errors > 1 then print_newline ()


let errorf ~header ?(location = none) ?(sub_errors = []) =
  Printf.ksprintf (fun message -> { location; header; message=(Some(message)); sub_errors})

let error ~header ?(location = none) ?(sub_errors = []) message =
  if message = "" then 
    {location; header; message=None; sub_errors}
  else
    {location; header; message=(Some message); sub_errors}

