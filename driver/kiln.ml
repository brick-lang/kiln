open Core.Std

let fire =
  Command.basic 
    ~summary:"Use kiln to compile the project"
    Command.Spec.( empty +> anon ("file" %: file ) ) 
    (fun file () -> (* Driver.process_file file *) ())

let parse =
  Command.basic 
    ~summary:"Use kiln to parse a file"
    Command.Spec.(
      empty +>
      anon ("file" %: file) +>
      (flag "-fmt" (optional_with_default "human" string) 
         ~doc:"string The formatter to use.\n Valid options: human, json")
    )
    (fun file format () -> 
       (* Fail fast before we start parsing *)
       if not (List.mem ["human"; "json"] format) then begin
         Printf.fprintf stderr "Error: %s is not a valid parse tree output format\n" format;
         exit 1;
       end;
       let parsed_file = 
         match Driver.parse_file file with
         | Some pf -> pf
         | None -> exit 1
       in
       match format with 
       | "human" ->
           Frontend.ParseTree.Printers.Human.implementation Format.std_formatter parsed_file

       | "json" -> 
           parsed_file
           |> Frontend.ParseTree.Printers.JSON.implementation 
           |> Yojson.Basic.pretty_to_string
           |> print_string

       | _ -> assert false
    )


let glaze =
  Command.basic ~summary:"Start the interactive toplevel"
    Command.Spec.(empty +> anon (maybe ("file" %: file)))
    (fun file () -> 
       match file with 
       | None -> print_string "TODO: Starting toplevel using linenoise\n"
       | Some f -> print_string "You loaded a file! Yay!" )

let command =
  Command.group ~summary:"The Brick reference compiler" [
    ("fire", fire); 
    ("glaze", glaze);
    ("parse", parse)
  ]

let () = Command.run ~version:"0.1" command
