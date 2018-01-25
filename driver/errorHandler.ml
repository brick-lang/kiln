

let unclosed opening_name opening_sp opening_ep closing_name closing_sp closing_ep =
  Core.Queue.enqueue Error.errors (Error.Unmatched(symbol_rloc opening_sp opening_ep, opening_name,
                                                   symbol_rloc closing_sp closing_ep, closing_name))

let unexpected ?(suggestion="") name opening closing  =
  Core.Queue.enqueue Error.errors (Error.Not_expecting(symbol_rloc opening closing, name, suggestion))

let expected ?(suggestion="") name opening closing =
  Core.Queue.enqueue Error.errors (Error.Expecting(symbol_rloc opening closing, name, suggestion))
