open Core
open Cmdliner


let fire_docstring = "Compile a project or file"
let fire_cmd =
  let files = Arg.(value & pos_all file [] & info [] ~docv:"FILE or DIR") in
  let exits = Term.default_exits in
  Term.(const Driver.codegen_files $ files), Term.info "fire" ~doc:fire_docstring ~exits

let parse_docstring = "Parse a file"
let parse_cmd =
  let module PrtFmt = struct type printer_format = Human | JSON end in
  let open PrtFmt in
  let format = Arg.(value & opt (some (enum [("human", Human); ("json", JSON)])) (Some Human) &
                    info  ["f"] ~docv:"FORMAT"  ~doc:"The formatter to use. Valid parse tree output formats: human, json")
  in
  let file = Arg.(value & (pos 0 non_dir_file) "" & info [] ~docv:"FILE") in
  let parse file format =
    let parsed_file =
      match Driver.parse_file file with
      | Some pf -> pf
      | None -> exit 1
    in
    match Option.value_exn format with
    | Human ->
        Frontend.ParseTree.Printers.Human.implementation Format.std_formatter parsed_file

    | JSON ->
        parsed_file
        |> Frontend.ParseTree.Printers.JSON.implementation
        |> Yojson.Basic.pretty_to_string
        |> print_string
  in
  Term.(const parse $ file $ format), Term.info "parse" ~doc:parse_docstring


let glaze_docstring = "Start the interactive toplevel"
let glaze_cmd =
  let doc = glaze_docstring in
  let file = Arg.(value & pos_all file [] & info [] ~docv:"FILE or DIR") in
  let run = function
    | [] -> print_endline "TODO: Starting toplevel using linenoise"
    | [f] -> print_endline "You loaded a file! Yay!"
    | _ -> assert false
  in
  Term.(const run $ file), Term.info "glaze" ~doc


let default_cmd =
  let doc = "the Brick reference compiler" in
  let usage () =
    Printf.ksprintf (fun s -> print_endline s; Out_channel.flush stdout) @@ Scanf.format_from_string (String.concat_array ~sep:"\n" [|
        "usage: kiln [--version][--help]";
        "            <command> [<args>]";
        "";
        "These are common Kiln commands used in various situations:";
        "    fire      " ^ fire_docstring;
        "    glaze     " ^ glaze_docstring;
        "    parse     " ^ parse_docstring;
        "";
        "See 'kiln help <command>' for more information on a specific command."|]) ""
  in
  Term.(pure usage $ pure ()),
  Term.info "kiln" ~version:"v0.0.1" ~doc

let cmds = [
  fire_cmd;
  parse_cmd;
  glaze_cmd
]

let () = Term.(exit @@ eval_choice default_cmd cmds)
