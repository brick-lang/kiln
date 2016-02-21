(** Identity function  *)
let id x = x 

(** Constant function *)
let const x _ = x

let seq  _ x = x

(** Function composition *)
(* OCaml doesn't let us use the (.) operator, so I just use ( *.* ), *)
(* which has a higher precedence than "@@" and "|>" *)
let ( *.* ) f g = fun x -> f (g x)

(** Flip takes its (first) two arguments in the reverse order of f *)
let flip f x y = f y x
let flip2 f y z x = f x y z
let flip3 f x y z w = f w x y z


(** The application operator. It's redundant, but in language grammars (like for Brick) it's 
  * given low, right-assoc precedence, allowing omission of parens *)
(* let (@@) f x = f x *)
