open Core.Std

let fire =
  Command.basic ~summary:"Use kiln to compile the project"
    Command.Spec.(
      empty
      +> anon ("file" %: string)
    )
    (fun file () -> print_string file )

let glaze =
  Command.basic ~summary:"Start the interactive toplevel"
    Command.Spec.(
      empty
      (* +> option ("file" %: string) *)
    )
    (fun (* file *) () -> () )

let command =
  Command.group ~summary:"The Brick compiler and interpreter"
    [ "fire", fire; "glaze",  glaze]

let () = Command.run command
