exception Error

type token = 
  | UNIT
  | UNDENT
  | TYPE of (
# 54 "parser.mly"
       (string)
# 10 "parser.ml"
)
  | TRUE
  | STRING of (
# 53 "parser.mly"
       (string)
# 16 "parser.ml"
)
  | SPACESHIP
  | SEMICOLON
  | RPAREN
  | RIGHT_STAB
  | RIGHT_FAT
  | RIGHT_CURVY
  | RCURLY
  | RBRACKET
  | PLUS
  | PIPE
  | NEWLINE
  | MINUS
  | LPAREN
  | LET
  | LEFT_STAB
  | LEFT_FAT
  | LEFT_CURVY
  | LCURLY
  | LBRACKET
  | INTEGER of (
# 51 "parser.mly"
       (int)
# 40 "parser.ml"
)
  | INDENT
  | IDENT of (
# 55 "parser.mly"
       (string)
# 46 "parser.ml"
)
  | FWD_SLASH
  | FUNCTION
  | FLOAT of (
# 52 "parser.mly"
       (float)
# 53 "parser.ml"
)
  | FALSE
  | EXTERN
  | EQUALS
  | EOS
  | DOUBLE_EQUALS
  | COMMA
  | COLON
  | BCK_SLASH
  | BANG
  | ASTERISK

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState27
  | MenhirState26
  | MenhirState16
  | MenhirState15
  | MenhirState12
  | MenhirState7
  | MenhirState2
  | MenhirState1
  | MenhirState0


# 1 "parser.mly"
  
open Ast
open Lexing

let parse_error s =
  begin
    try
      let start_pos = Parsing.symbol_start_pos ()
      and end_pos = Parsing.symbol_end_pos () in
      Printf.printf "File \"%s\", line %d, characters %d-%d: \n"
          start_pos.pos_fname
          start_pos.pos_lnum
          (start_pos.pos_cnum - start_pos.pos_bol)
          (end_pos.pos_cnum - start_pos.pos_bol)
    with Invalid_argument(_) -> ()
  end;
  Printf.printf "Syntax error: %s\n" s;
  raise Parsing.Parse_error

# 107 "parser.ml"
let _eRR =
  Error

let rec _menhir_goto_sep : _menhir_env -> 'ttv_tail -> 'tv_sep -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv151 * _menhir_state * 'tv_term_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
    let (_v : 'tv_sep) = _v in
    ((Printf.fprintf Pervasives.stderr "State 25:\n%!";
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv149 * _menhir_state * 'tv_term_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
    let (_ : 'tv_sep) = _v in
    ((Printf.fprintf Pervasives.stderr "Reducing production term_expr -> term_expr expr sep \n%!";
    let ((_menhir_stack, _menhir_s, te), _, e) = _menhir_stack in
    let _v : 'tv_term_expr = 
# 136 "parser.mly"
                                  ( te@[e] )
# 125 "parser.ml"
     in
    _menhir_goto_term_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv150)) : 'freshtv152)

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv147 * _menhir_state * 'tv_term_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
    ((Printf.fprintf Pervasives.stderr "State 22:\n%!";
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv145 * _menhir_state * 'tv_term_expr) * _menhir_state * 'tv_expr) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | NEWLINE ->
        Printf.fprintf Pervasives.stderr "Shifting (NEWLINE) to state 24\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 24:\n%!";
        let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "Reducing production sep -> NEWLINE \n%!";
        let _v : 'tv_sep = 
# 144 "parser.mly"
            ( )
# 153 "parser.ml"
         in
        _menhir_goto_sep _menhir_env _menhir_stack _v) : 'freshtv136)) : 'freshtv138)
    | SEMICOLON ->
        Printf.fprintf Pervasives.stderr "Shifting (SEMICOLON) to state 23\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv141) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 23:\n%!";
        let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv139) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "Reducing production sep -> SEMICOLON \n%!";
        let _v : 'tv_sep = 
# 145 "parser.mly"
              ( )
# 168 "parser.ml"
         in
        _menhir_goto_sep _menhir_env _menhir_stack _v) : 'freshtv140)) : 'freshtv142)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv143 * _menhir_state * 'tv_term_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)) : 'freshtv146)) : 'freshtv148)

and _menhir_goto_block : _menhir_env -> 'ttv_tail -> 'tv_block -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv133 * _menhir_state * 'tv_var_bind) = Obj.magic _menhir_stack in
    let (_v : 'tv_block) = _v in
    ((Printf.fprintf Pervasives.stderr "State 29:\n%!";
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv131 * _menhir_state * 'tv_var_bind) = Obj.magic _menhir_stack in
    let (bl : 'tv_block) = _v in
    ((Printf.fprintf Pervasives.stderr "Reducing production lambda -> var_bind block \n%!";
    let (_menhir_stack, _menhir_s, vb) = _menhir_stack in
    let _v : 'tv_lambda = 
# 86 "parser.mly"
                               ( let (vb : variable list) = vb in Function(Prototype("", vb), bl) )
# 194 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv129) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_lambda) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState27 | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111 * _menhir_state * 'tv_lambda) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 21:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * _menhir_state * 'tv_lambda) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "Reducing production expr -> lambda \n%!";
        let (_menhir_stack, _menhir_s, l) = _menhir_stack in
        let _v : 'tv_expr = 
# 83 "parser.mly"
                ( Lambda(l) )
# 213 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv110)) : 'freshtv112)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv127 * _menhir_state * 'tv_lambda) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 31:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv125 * _menhir_state * 'tv_lambda) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | NEWLINE ->
            Printf.fprintf Pervasives.stderr "Shifting (NEWLINE) to state 32\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv121 * _menhir_state * 'tv_lambda) = Obj.magic _menhir_stack in
            ((Printf.fprintf Pervasives.stderr "State 32:\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv119 * _menhir_state * 'tv_lambda) = Obj.magic _menhir_stack in
            ((Printf.fprintf Pervasives.stderr "Reducing production stmt -> lambda NEWLINE \n%!";
            let (_menhir_stack, _menhir_s, l) = _menhir_stack in
            let _v : (
# 73 "parser.mly"
       (Ast.toplevel)
# 238 "parser.ml"
            ) = 
# 77 "parser.mly"
                       ( TopFun(l) )
# 242 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv117) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 73 "parser.mly"
       (Ast.toplevel)
# 250 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv115) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 73 "parser.mly"
       (Ast.toplevel)
# 258 "parser.ml"
            )) = _v in
            ((Printf.fprintf Pervasives.stderr "State 30:\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv113) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_1 : (
# 73 "parser.mly"
       (Ast.toplevel)
# 267 "parser.ml"
            )) = _v in
            ((Printf.fprintf Pervasives.stderr "Accepting\n%!";
            Obj.magic _1) : 'freshtv114)) : 'freshtv116)) : 'freshtv118)) : 'freshtv120)) : 'freshtv122)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv123 * _menhir_state * 'tv_lambda) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)) : 'freshtv126)) : 'freshtv128)
    | _ ->
        _menhir_fail ()) : 'freshtv130)) : 'freshtv132)) : 'freshtv134)

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 51 "parser.mly"
       (int)
# 285 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 18:\n%!";
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv107) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (i : (
# 51 "parser.mly"
       (int)
# 296 "parser.ml"
    )) = _v in
    ((Printf.fprintf Pervasives.stderr "Reducing production expr -> INTEGER \n%!";
    let _v : 'tv_expr = 
# 80 "parser.mly"
                ( let (i : expr) = Integer(i) in i )
# 302 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv108)

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 55 "parser.mly"
       (string)
# 309 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 19:\n%!";
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv105) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (i : (
# 55 "parser.mly"
       (string)
# 320 "parser.ml"
    )) = _v in
    ((Printf.fprintf Pervasives.stderr "Reducing production expr -> IDENT \n%!";
    let _v : 'tv_expr = 
# 82 "parser.mly"
                ( let (i : expr) = Ident(i) in i )
# 326 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv106)

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 52 "parser.mly"
       (float)
# 333 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 20:\n%!";
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv103) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (f : (
# 52 "parser.mly"
       (float)
# 344 "parser.ml"
    )) = _v in
    ((Printf.fprintf Pervasives.stderr "Reducing production expr -> FLOAT \n%!";
    let _v : 'tv_expr = 
# 81 "parser.mly"
                ( let (f : expr) = Float(f) in f )
# 350 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv104)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_var : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_var -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_var) = _v in
        ((Printf.fprintf Pervasives.stderr "State 9:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (v : 'tv_var) = _v in
        ((Printf.fprintf Pervasives.stderr "Reducing production arg_list -> var \n%!";
        let _v : 'tv_arg_list = 
# 94 "parser.mly"
            ( [v] )
# 376 "parser.ml"
         in
        _menhir_goto_arg_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv96)) : 'freshtv98)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_var) = _v in
        ((Printf.fprintf Pervasives.stderr "State 13:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99 * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (v : 'tv_var) = _v in
        ((Printf.fprintf Pervasives.stderr "Reducing production arg_list -> arg_list COMMA var \n%!";
        let (_menhir_stack, _menhir_s, a) = _menhir_stack in
        let _v : 'tv_arg_list = 
# 93 "parser.mly"
                                  ( a @ [v] )
# 394 "parser.ml"
         in
        _menhir_goto_arg_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv100)) : 'freshtv102)
    | _ ->
        _menhir_fail ()

and _menhir_goto_term_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_term_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85) * _menhir_state * 'tv_term_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 16:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83) * _menhir_state * 'tv_term_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | FLOAT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | IDENT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (IDENT) to state 19\n%!";
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | INTEGER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (INTEGER) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
        | PIPE ->
            Printf.fprintf Pervasives.stderr "Shifting (PIPE) to state 1\n%!";
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | RCURLY ->
            Printf.fprintf Pervasives.stderr "Shifting (RCURLY) to state 17\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv81) * _menhir_state * 'tv_term_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState16 in
            ((Printf.fprintf Pervasives.stderr "State 17:\n%!";
            let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv79) * _menhir_state * 'tv_term_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((Printf.fprintf Pervasives.stderr "Reducing production block -> LCURLY term_expr RCURLY \n%!";
            let (_menhir_stack, _, te) = _menhir_stack in
            let _v : 'tv_block = 
# 140 "parser.mly"
                                   ( Block(te) )
# 441 "parser.ml"
             in
            _menhir_goto_block _menhir_env _menhir_stack _v) : 'freshtv80)) : 'freshtv82)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16) : 'freshtv84)) : 'freshtv86)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv93) * _menhir_state * 'tv_term_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 27:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91) * _menhir_state * 'tv_term_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | FLOAT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | IDENT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (IDENT) to state 19\n%!";
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | INTEGER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (INTEGER) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | PIPE ->
            Printf.fprintf Pervasives.stderr "Shifting (PIPE) to state 1\n%!";
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | UNDENT ->
            Printf.fprintf Pervasives.stderr "Shifting (UNDENT) to state 28\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv89) * _menhir_state * 'tv_term_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState27 in
            ((Printf.fprintf Pervasives.stderr "State 28:\n%!";
            let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv87) * _menhir_state * 'tv_term_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((Printf.fprintf Pervasives.stderr "Reducing production block -> INDENT term_expr UNDENT \n%!";
            let (_menhir_stack, _, te) = _menhir_stack in
            let _v : 'tv_block = 
# 141 "parser.mly"
                                   ( Block(te) )
# 486 "parser.ml"
             in
            _menhir_goto_block _menhir_env _menhir_stack _v) : 'freshtv88)) : 'freshtv90)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27) : 'freshtv92)) : 'freshtv94)
    | _ ->
        _menhir_fail ()

and _menhir_goto_type_annot : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_type_annot -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state * (
# 55 "parser.mly"
       (string)
# 505 "parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_type_annot) = _v in
        ((Printf.fprintf Pervasives.stderr "State 5:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71 * _menhir_state * (
# 55 "parser.mly"
       (string)
# 514 "parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (t : 'tv_type_annot) = _v in
        ((Printf.fprintf Pervasives.stderr "Reducing production var -> IDENT type_annot \n%!";
        let (_menhir_stack, _menhir_s, i) = _menhir_stack in
        let _v : 'tv_var = 
# 127 "parser.mly"
                              ( Plain_Var(i, t) )
# 523 "parser.ml"
         in
        _menhir_goto_var _menhir_env _menhir_stack _menhir_s _v) : 'freshtv72)) : 'freshtv74)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state) * (
# 55 "parser.mly"
       (string)
# 531 "parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_type_annot) = _v in
        ((Printf.fprintf Pervasives.stderr "State 8:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state) * (
# 55 "parser.mly"
       (string)
# 540 "parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (t : 'tv_type_annot) = _v in
        ((Printf.fprintf Pervasives.stderr "Reducing production var -> BANG IDENT type_annot \n%!";
        let ((_menhir_stack, _menhir_s), i) = _menhir_stack in
        let _v : 'tv_var = 
# 126 "parser.mly"
                                    ( Ref_Var(i, t) )
# 549 "parser.ml"
         in
        _menhir_goto_var _menhir_env _menhir_stack _menhir_s _v) : 'freshtv76)) : 'freshtv78)
    | _ ->
        _menhir_fail ()

and _menhir_reduce14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "Reducing production term_expr -> \n%!";
    let _v : 'tv_term_expr = 
# 135 "parser.mly"
                ( [ ] )
# 561 "parser.ml"
     in
    _menhir_goto_term_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "Reducing production type_annot -> \n%!";
    let _v : 'tv_type_annot = 
# 131 "parser.mly"
                ( Unknown_Type )
# 571 "parser.ml"
     in
    _menhir_goto_type_annot _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 3:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv69 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | TYPE _v ->
        Printf.fprintf Pervasives.stderr "Shifting (TYPE) to state 4\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 54 "parser.mly"
       (string)
# 591 "parser.ml"
        )) = _v in
        ((Printf.fprintf Pervasives.stderr "State 4:\n%!";
        let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * _menhir_state) = Obj.magic _menhir_stack in
        let (t : (
# 54 "parser.mly"
       (string)
# 600 "parser.ml"
        )) = _v in
        ((Printf.fprintf Pervasives.stderr "Reducing production type_annot -> COLON TYPE \n%!";
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : 'tv_type_annot = 
# 132 "parser.mly"
                    ( Known_Type(t) )
# 607 "parser.ml"
         in
        _menhir_goto_type_annot _menhir_env _menhir_stack _menhir_s _v) : 'freshtv64)) : 'freshtv66)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv67 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)) : 'freshtv70)

and _menhir_goto_arg_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_arg_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv61 * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
    ((Printf.fprintf Pervasives.stderr "State 10:\n%!";
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv59 * _menhir_state) * _menhir_state * 'tv_arg_list) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | COMMA ->
        Printf.fprintf Pervasives.stderr "Shifting (COMMA) to state 12\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 12:\n%!";
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37 * _menhir_state * 'tv_arg_list) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | BANG ->
            Printf.fprintf Pervasives.stderr "Shifting (BANG) to state 6\n%!";
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | IDENT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (IDENT) to state 2\n%!";
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12) : 'freshtv38)) : 'freshtv40)
    | PIPE ->
        Printf.fprintf Pervasives.stderr "Shifting (PIPE) to state 11\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 11:\n%!";
        let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv53 * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "Reducing production var_bind -> PIPE arg_list PIPE \n%!";
        let ((_menhir_stack, _menhir_s), _, a) = _menhir_stack in
        let _v : 'tv_var_bind = 
# 89 "parser.mly"
                             ( let (a : variable list) = a in a )
# 665 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_var_bind) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * _menhir_state * 'tv_var_bind) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 14:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv47 * _menhir_state * 'tv_var_bind) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | INDENT ->
            Printf.fprintf Pervasives.stderr "Shifting (INDENT) to state 26\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv41) = Obj.magic _menhir_stack in
            ((Printf.fprintf Pervasives.stderr "State 26:\n%!";
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack) MenhirState26) : 'freshtv42)
        | LCURLY ->
            Printf.fprintf Pervasives.stderr "Shifting (LCURLY) to state 15\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv43) = Obj.magic _menhir_stack in
            ((Printf.fprintf Pervasives.stderr "State 15:\n%!";
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack) MenhirState15) : 'freshtv44)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv45 * _menhir_state * 'tv_var_bind) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)) : 'freshtv48)) : 'freshtv50)) : 'freshtv52)) : 'freshtv54)) : 'freshtv56)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv57 * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)) : 'freshtv60)) : 'freshtv62)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 55 "parser.mly"
       (string)
# 715 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 2:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv35 * _menhir_state * (
# 55 "parser.mly"
       (string)
# 725 "parser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | COLON ->
        Printf.fprintf Pervasives.stderr "Shifting (COLON) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | COMMA | PIPE ->
        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2) : 'freshtv36)

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 6:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv33 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (IDENT) to state 7\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 55 "parser.mly"
       (string)
# 756 "parser.ml"
        )) = _v in
        ((Printf.fprintf Pervasives.stderr "State 7:\n%!";
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv27 * _menhir_state) * (
# 55 "parser.mly"
       (string)
# 765 "parser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COLON ->
            Printf.fprintf Pervasives.stderr "Shifting (COLON) to state 3\n%!";
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | COMMA | PIPE ->
            _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv28)) : 'freshtv30)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)) : 'freshtv34)

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    Printf.fprintf Pervasives.stderr "Lookahead token is now %s (%d-%d)\n%!" (_menhir_print_token _tok) _menhir_env._menhir_startp.Lexing.pos_cnum _menhir_env._menhir_endp.Lexing.pos_cnum;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv9) * _menhir_state * 'tv_term_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv12)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv13) * _menhir_state * 'tv_term_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv16)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv19 * _menhir_state) * (
# 55 "parser.mly"
       (string)
# 832 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * (
# 55 "parser.mly"
       (string)
# 841 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv26)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 1:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv7 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | BANG ->
        Printf.fprintf Pervasives.stderr "Shifting (BANG) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | IDENT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (IDENT) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | COMMA | PIPE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState1 in
        ((Printf.fprintf Pervasives.stderr "Reducing production arg_list -> \n%!";
        let _v : 'tv_arg_list = 
# 95 "parser.mly"
                ( [ ] )
# 878 "parser.ml"
         in
        _menhir_goto_arg_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv6)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1) : 'freshtv8)

and _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ASTERISK ->
        "ASTERISK"
    | BANG ->
        "BANG"
    | BCK_SLASH ->
        "BCK_SLASH"
    | COLON ->
        "COLON"
    | COMMA ->
        "COMMA"
    | DOUBLE_EQUALS ->
        "DOUBLE_EQUALS"
    | EOS ->
        "EOS"
    | EQUALS ->
        "EQUALS"
    | EXTERN ->
        "EXTERN"
    | FALSE ->
        "FALSE"
    | FLOAT _ ->
        "FLOAT"
    | FUNCTION ->
        "FUNCTION"
    | FWD_SLASH ->
        "FWD_SLASH"
    | IDENT _ ->
        "IDENT"
    | INDENT ->
        "INDENT"
    | INTEGER _ ->
        "INTEGER"
    | LBRACKET ->
        "LBRACKET"
    | LCURLY ->
        "LCURLY"
    | LEFT_CURVY ->
        "LEFT_CURVY"
    | LEFT_FAT ->
        "LEFT_FAT"
    | LEFT_STAB ->
        "LEFT_STAB"
    | LET ->
        "LET"
    | LPAREN ->
        "LPAREN"
    | MINUS ->
        "MINUS"
    | NEWLINE ->
        "NEWLINE"
    | PIPE ->
        "PIPE"
    | PLUS ->
        "PLUS"
    | RBRACKET ->
        "RBRACKET"
    | RCURLY ->
        "RCURLY"
    | RIGHT_CURVY ->
        "RIGHT_CURVY"
    | RIGHT_FAT ->
        "RIGHT_FAT"
    | RIGHT_STAB ->
        "RIGHT_STAB"
    | RPAREN ->
        "RPAREN"
    | SEMICOLON ->
        "SEMICOLON"
    | SPACESHIP ->
        "SPACESHIP"
    | STRING _ ->
        "STRING"
    | TRUE ->
        "TRUE"
    | TYPE _ ->
        "TYPE"
    | UNDENT ->
        "UNDENT"
    | UNIT ->
        "UNIT"

and stmt : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 73 "parser.mly"
       (Ast.toplevel)
# 974 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = lexer lexbuf in
      Printf.fprintf Pervasives.stderr "Lookahead token is now %s (%d-%d)\n%!" (_menhir_print_token _tok) lexbuf.Lexing.lex_start_p.Lexing.pos_cnum lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum;
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_startp = lexbuf.Lexing.lex_start_p;
        _menhir_endp = lexbuf.Lexing.lex_curr_p;
        _menhir_shifted = max_int;
        }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = () in
    ((Printf.fprintf Pervasives.stderr "State 0:\n%!";
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | PIPE ->
        Printf.fprintf Pervasives.stderr "Shifting (PIPE) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2)) : 'freshtv4))



