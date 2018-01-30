open Common
(* Auxiliary type for reporting syntax errors *)
type 'a loc = 'a Location.loc

type error =
  | Unmatched of string loc * string loc
  | Expecting of (string * string) loc
  | Not_expecting of (string * string) loc
  | Other of Location.t

let prepare_error error = 
  let errorf = Location.errorf ~header:"Syntax Error" in
  match error with
  | Unmatched(opening, closing) ->
      errorf ~location:closing.loc
        ~sub_errors:[
          errorf ~location:opening.loc
            "This '%s' might be unmatched" opening.txt
        ]
        "'%s' expected" closing.txt

  | Expecting {txt=(nonterm, suggestion); loc} ->
      if Core.String.is_empty suggestion then
        errorf ~location:loc "Expected %s." nonterm
      else
        errorf ~location:loc "Expected %s." nonterm ~sub_errors:[
          errorf ~location:loc "Maybe you meant to use %s?" suggestion
        ]

  | Not_expecting {txt=(nonterm, suggestion); loc} ->
      if Core.String.is_empty suggestion then
        errorf ~location:loc "Unexpected %s." nonterm
      else
        errorf ~location:loc "Unexpected %s." nonterm ~sub_errors:[
          errorf ~location:loc "Maybe you meant to use %s?" suggestion
        ]

  | Other location ->
      Location.error ~header:"Syntax Error" ~location ""

let unclosed opening_name opening_sp opening_ep closing_name closing_sp closing_ep =
  Unmatched({loc=Location.rloc opening_sp opening_ep; txt=opening_name},
            {loc=Location.rloc closing_sp closing_ep; txt=closing_name})

let unexpected ?(suggestion="") name opening closing  =
  Not_expecting{loc=Location.rloc opening closing; txt=(name, suggestion)}

let expected ?(suggestion="") name opening closing =
  Expecting{loc=Location.rloc opening closing; txt=(name, suggestion)}
