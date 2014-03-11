open Format

module Error = struct

exception Exit of int

let errf f = 
  print_flush(); 
  open_vbox 0; 
  open_hvbox 0; f(); print_cut(); close_box(); print_newline();
  raise (Exit 1)

let err s = errf (fun()-> print_string "Error: "; print_string s; print_newline())

let warning s =
  print_string "Warning: "; print_string s;
  print_newline()
end
