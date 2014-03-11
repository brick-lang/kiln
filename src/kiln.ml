open Core.Std

let fire =
  Command.basic ~summary:"Use kiln to compile the project"
    Command.Spec.(
      empty
      +> anon ("file" %: file))
    (fun file () -> Driver.process_file file)

let glaze =
  Command.basic ~summary:"Start the interactive toplevel"
    Command.Spec.(
      empty
       +> anon (maybe ("file" %: file)))
    (fun file () -> 
       match file with 
       | None -> print_string "TODO: Starting toplevel using linenoise\n"
       | Some f -> print_string "You loaded a file! Yay!" )

let command =
  Command.group ~summary:"The Brick compiler and interpreter"
    [ "fire", fire; "glaze",  glaze]

let () = Command.run command
