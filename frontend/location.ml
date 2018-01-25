
(* Originally from the OCaml compiler, which is licensed under the QPL.
 * This has been adapted to use Sedlexing, and will be heavily modified
 * in the future. *)

open Sedlexing
open Core

let absname = ref false
(* This reference should be in Clflags, but it would create an additional
   dependency and make bootstrapping Camlp4 more difficult. *)

type t = {
  loc_start: position;
  loc_end: position;
  loc_ghost: bool
};;

let in_file name =
  let loc = {
    file_name = name;
    line_number = 1;
    line_offset = 0;
    buffer_offset = -1;
  } in
  { loc_start = loc; loc_end = loc; loc_ghost = true }
;;

let none = in_file "_none_";;

let curr lexbuf = {
  loc_start = lexbuf.start_pos;
  loc_end = lexbuf.curr_pos;
  loc_ghost = false
};;

let init lexbuf fname =
  lexbuf.curr_pos <- {
    file_name = fname;
    line_number = 1;
    line_offset = 0;
    buffer_offset = 0;
  }
;;

let input_name = ref "_none_"
let input_lexbuf = ref (None : lexbuf option)

(* Terminal info *)

let status = ref Terminfo.Uninitialised

let num_loc_lines = ref 0 (* number of lines already printed after input *)

(* let explode s = *)
(*   let rec expl i l = *)
(*     if i < 0 then l else *)
(*       expl (i - 1) (s.[i] :: l) in *)
(*   expl (String.length s - 1) [];; *)

(* let camomile_implode l = *)
(*   let result = Camomile.UnicodeString. .create (List.length l) in *)
(*   let rec imp i = function *)
(*     | [] -> result *)
(*     | c :: l -> result.[i] <- c; imp (i + 1) l in *)
(*   imp 0 l;; *)

open Textutils.Std


let highlight_textutils (header: unit -> unit ) lb loc = 
  (* Char 0 is at offset -lb.offset in lb.lex_buffer. *)
  let pos0 = -lb.offset in
  (* Do nothing if the buffer does not contain the whole phrase. *)
  (* if pos0 < 0 then begin *)
  (*   print_endline @@ string_of_int pos0; *)
  (*   print_endline @@ string_of_int lb.len; *)
  (*   raise Exit *)
  (* end; *)
  if pos0 >= 0 then
    let end_pos = lb.len - pos0 - 1 in
    (* Determine line numbers for the start and end points *)
    let line_start = ref 0 and line_end = ref 0 in
    for pos = 0 to end_pos do
      if Array.get lb.buf (pos + pos0) = Char.to_int '\n' then begin
        if loc.loc_start.buffer_offset > pos then incr line_start;
        if loc.loc_end.buffer_offset   > pos then incr line_end;
      end
    done;
    (* Print character location (useful for Emacs) *)
    Console.Ansi.printf [`Bright] "%s:%d:%d" lb.curr_pos.file_name (!line_start + 1)
      loc.loc_start.buffer_offset;
    header ();
    Out_channel.newline stdout;
    print_string "  ";
    Out_channel.flush stdout;
    let line = ref 0 in
    (* let pos = ref 0 in *)
    let pos_at_bol = ref 0 in
    let segment = Array.map ~f:BatUChar.of_int @@ Array.slice lb.buf pos0 end_pos in
    (* let ustring = BatText.implode @@ Array.to_list segment in *)
    (* BatText.write_text BatIO.stdout ustring; *)
    let utf8_handler pos uchar =
      let print_uchar = BatText.write_char BatIO.stdout in begin
        if !line = !line_start && !line = !line_end then
          (* loc is on one line: print whole line *)
          print_uchar uchar
        else if !line = !line_start then
          (* first line of multiline loc:
           * print a dot for each char before loc_start *)
          (* if pos < loc.loc_start.buffer_offset then *)
          (*   print_char '.' *)
          (* else *)
          print_uchar uchar
        else if !line = !line_end then
          (* last line of multiline loc: print a dot for each char
             after loc_end, even whitespaces *)
          if pos < loc.loc_end.buffer_offset
          then print_uchar uchar
          else Out_channel.output_char stdout '.'
        else if !line > !line_start && !line < !line_end then
          (* intermediate line of multiline loc: print whole line *)
          print_uchar uchar
      end;
    in
    let underliner start stop =
      if start = stop then
        Console.Ansi.printf [`Green] "^"
      else
        for _i = start to (stop - 1) do
          if _i = start then
            Console.Ansi.printf [`Green] "^"
          else
            Console.Ansi.printf [`Green] "~"
        done
    in
    let ascii_handler pos uchar =
      let char = BatUChar.char_of uchar in
      let start_offset = loc.loc_start.buffer_offset in
      let end_offset = loc.loc_end.buffer_offset in
      if char = '\n' then begin
        if !line = !line_start && !line = !line_end then begin
          (* loc is on one line: underline location *)
          Out_channel.newline stdout;
          print_string "  ";
          for _i = !pos_at_bol to start_offset - 1 do
            Out_channel.output_char stdout ' ';
          done;
          underliner start_offset end_offset;
        end;
        if !line >= !line_start && !line < !line_end then begin
          Out_channel.newline stdout;
          if pos < end_offset then print_string "  "
        end;
        incr line;
        pos_at_bol := pos + 1;
      end
      else if char = '\r' then ()
      else utf8_handler pos uchar
    in
    Array.iteri segment ~f:(fun pos uchar ->
        if BatUChar.is_ascii uchar
        then ascii_handler pos uchar
        else utf8_handler pos uchar);
    Out_channel.newline stdout;

    (* Highlight the location using one of the supported modes. *)

    (* let rec highlight_locations lb = function *)
    (*   | [] -> () *)
    (*   | x::xs ->  *)
    (*     highlight_textutils lb x; *)
    (*     highlight_locations lb xs *)

    (* Print the location in some way or another *)

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

let show_filename file =
  if !absname then absolute_path file else file

let print_filename ppf file =
  Format.fprintf ppf "%s" (show_filename file)

let reset () =
  num_loc_lines := 0

let (msg_file, msg_line, msg_chars, msg_to, msg_colon) =
  ("File \"", "\", line ", ", characters ", "-", ":")

(* return file, line, char from the given position *)
let get_pos_info pos =
  (pos.file_name, pos.line_number, pos.buffer_offset - pos.line_offset)
;;

(* let print_error_cur_file ppf = print_error ppf (in_file !input_name);; *)

let print_warning lb loc w =
  if Warnings.is_active w then begin
    (* let printw ppf w = *)
    (*   let n = Warnings.print w in *)
    (*   num_loc_lines := !num_loc_lines + n *)
    (* in *)
    (* print lb loc; *)
    (* fprintf ppf "Warning %a@." printw w; *)
    (* pp_print_flush ppf (); *)
    (* incr num_loc_lines; *)
  end
;;

(* let prerr_warning loc w = print_warning loc err_formatter w;; *)

let echo_eof () =
  print_newline ();
  incr num_loc_lines

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

let rec print_error lb ?(sub_error=false) { header; message; location; sub_errors} =
  let header_helper () : unit = 
    Console.Ansi.printf [`Red; `Bright] " Error: ";
    Console.Ansi.printf [`Bright] "%s" header;
    ()
  in
  if not sub_error then
    highlight_textutils header_helper lb location;
  (match message with
   | Some m -> 
       if not sub_error then
         Console.Ansi.printf [`Bright] "Reason: "
       else
         Console.Ansi.printf [`Bright] "Suggestion: ";
       print_endline m
   | None -> ());
  if not @@ List.is_empty sub_errors then begin
    ignore @@ List.map ~f:(print_error lb ~sub_error:true) sub_errors;
    print_newline ()
  end



let errorf ~header ?(location = none) ?(sub_errors = []) =
  Printf.ksprintf (fun message -> { location; header; message=(Some(message)); sub_errors})

let error ~header ?(location = none) ?(sub_errors = []) message =
  if message = "" then 
    {location; header; message=None; sub_errors}
  else
    {location; header; message=(Some message); sub_errors}

let error_of_exn : (exn -> error option) list ref = ref []

let register_error_of_exn f = error_of_exn := f :: !error_of_exn

let error_of_exn exn =
  let rec loop = function
    | [] -> None
    | f :: rest ->
        match f exn with
        | Some _ as r -> r
        | None -> loop rest
  in
  loop !error_of_exn

exception Error of error

let () = 
  register_error_of_exn (function Error e -> Some e 
                                | _ -> None)
