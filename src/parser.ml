exception Error

type token = 
  | UNDENT
  | TYPE of (
# 54 "parser.mly"
       (string)
# 9 "parser.ml"
)
  | TRUE
  | STRING of (
# 53 "parser.mly"
       (string)
# 15 "parser.ml"
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
# 39 "parser.ml"
)
  | INDENT
  | IDENT of (
# 55 "parser.mly"
       (string)
# 45 "parser.ml"
)
  | FWD_SLASH
  | FUNCTION
  | FLOAT of (
# 52 "parser.mly"
       (float)
# 52 "parser.ml"
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
  | MenhirState41
  | MenhirState40
  | MenhirState35
  | MenhirState34
  | MenhirState32
  | MenhirState29
  | MenhirState28
  | MenhirState27
  | MenhirState26
  | MenhirState21
  | MenhirState20
  | MenhirState19
  | MenhirState17
  | MenhirState14
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

# 116 "parser.ml"
let _eRR =
  Error

let rec _menhir_reduce10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_lambda -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s l ->
    Printf.fprintf Pervasives.stderr "Reducing production expr -> lambda \n%!";
    let _v : 'tv_expr = 
# 80 "parser.mly"
                ( Lambda(l)  )
# 126 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce11 : _menhir_env -> ('ttv_tail * _menhir_state * 'tv_var_bind) * _menhir_state * 'tv_list_sep_ -> _menhir_state -> 'tv_block -> 'ttv_return =
  fun _menhir_env _menhir_stack _ bl ->
    Printf.fprintf Pervasives.stderr "Reducing production lambda -> var_bind list(sep) block \n%!";
    let ((_menhir_stack, _menhir_s, vb), _, _) = _menhir_stack in
    let _v : 'tv_lambda = 
# 83 "parser.mly"
                                    ( let (vb : variable list) = vb in Function(Prototype("", vb), bl) )
# 137 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv193) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_lambda) = _v in
    ((match _menhir_s with
    | MenhirState41 | MenhirState21 | MenhirState27 | MenhirState35 | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv179) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_lambda) = _v in
        ((Printf.fprintf Pervasives.stderr "State 31:\n%!";
        _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v) : 'freshtv180)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv181) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_lambda) = _v in
        ((Printf.fprintf Pervasives.stderr "State 45:\n%!";
        _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v) : 'freshtv182)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv191) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_lambda) = _v in
        ((Printf.fprintf Pervasives.stderr "State 49:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv189) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (l : 'tv_lambda) = _v in
        ((Printf.fprintf Pervasives.stderr "Reducing production stmt -> lambda \n%!";
        let _v : (
# 70 "parser.mly"
       (Ast.toplevel)
# 172 "parser.ml"
        ) = 
# 74 "parser.mly"
                ( TopFun(l) )
# 176 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv187) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 70 "parser.mly"
       (Ast.toplevel)
# 184 "parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv185) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 70 "parser.mly"
       (Ast.toplevel)
# 192 "parser.ml"
        )) = _v in
        ((Printf.fprintf Pervasives.stderr "State 48:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv183) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_1 : (
# 70 "parser.mly"
       (Ast.toplevel)
# 201 "parser.ml"
        )) = _v in
        ((Printf.fprintf Pervasives.stderr "Accepting\n%!";
        Obj.magic _1) : 'freshtv184)) : 'freshtv186)) : 'freshtv188)) : 'freshtv190)) : 'freshtv192)
    | _ ->
        _menhir_fail ()) : 'freshtv194)

and _menhir_reduce6 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "Reducing production block -> expr \n%!";
    let (_menhir_stack, _menhir_s, e) = _menhir_stack in
    let _v : 'tv_block = 
# 134 "parser.mly"
             ( e )
# 215 "parser.ml"
     in
    _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_block : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_block -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv175 * _menhir_state * 'tv_var_bind) * _menhir_state * 'tv_list_sep_) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_block) = _v in
        ((Printf.fprintf Pervasives.stderr "State 38:\n%!";
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v) : 'freshtv176)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv177 * _menhir_state * 'tv_var_bind) * _menhir_state * 'tv_list_sep_) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_block) = _v in
        ((Printf.fprintf Pervasives.stderr "State 47:\n%!";
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v) : 'freshtv178)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState41 | MenhirState21 | MenhirState35 | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv169 * _menhir_state * 'tv_term_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 32:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv167 * _menhir_state * 'tv_term_expr) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | NEWLINE ->
            Printf.fprintf Pervasives.stderr "Shifting (NEWLINE) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | SEMICOLON ->
            Printf.fprintf Pervasives.stderr "Shifting (SEMICOLON) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv168)) : 'freshtv170)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv171 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 37:\n%!";
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv172)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv173 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 46:\n%!";
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv174)
    | _ ->
        _menhir_fail ()

and _menhir_reduce5 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * 'tv_term_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    Printf.fprintf Pervasives.stderr "Reducing production block -> INDENT term_expr UNDENT \n%!";
    let ((_menhir_stack, _menhir_s), _, te) = _menhir_stack in
    let _v : 'tv_block = 
# 133 "parser.mly"
                                   ( Block(te) )
# 284 "parser.ml"
     in
    _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce4 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * 'tv_term_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    Printf.fprintf Pervasives.stderr "Reducing production block -> LCURLY term_expr RCURLY \n%!";
    let ((_menhir_stack, _menhir_s), _, te) = _menhir_stack in
    let _v : 'tv_block = 
# 132 "parser.mly"
                                   ( Block(te) )
# 295 "parser.ml"
     in
    _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 51 "parser.mly"
       (int)
# 302 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 23:\n%!";
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 55 "parser.mly"
       (string)
# 312 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 24:\n%!";
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 52 "parser.mly"
       (float)
# 322 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 25:\n%!";
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_reduce7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 51 "parser.mly"
       (int)
# 332 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s i ->
    Printf.fprintf Pervasives.stderr "Reducing production expr -> INTEGER \n%!";
    let _v : 'tv_expr = 
# 77 "parser.mly"
                ( Integer(i) )
# 339 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "Reducing production term_expr -> \n%!";
    let _v : 'tv_term_expr = 
# 126 "parser.mly"
                ( [ ] )
# 349 "parser.ml"
     in
    _menhir_goto_term_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 55 "parser.mly"
       (string)
# 356 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s i ->
    Printf.fprintf Pervasives.stderr "Reducing production expr -> IDENT \n%!";
    let _v : 'tv_expr = 
# 79 "parser.mly"
                ( Ident(i)   )
# 363 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 52 "parser.mly"
       (float)
# 370 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s f ->
    Printf.fprintf Pervasives.stderr "Reducing production expr -> FLOAT \n%!";
    let _v : 'tv_expr = 
# 78 "parser.mly"
                ( Float(f)   )
# 377 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_term_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_term_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv147 * _menhir_state) * _menhir_state * 'tv_term_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 21:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv145 * _menhir_state) * _menhir_state * 'tv_term_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | FLOAT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 25\n%!";
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | IDENT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (IDENT) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | INTEGER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (INTEGER) to state 23\n%!";
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | PIPE ->
            Printf.fprintf Pervasives.stderr "Shifting (PIPE) to state 1\n%!";
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | RCURLY ->
            Printf.fprintf Pervasives.stderr "Shifting (RCURLY) to state 22\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv143 * _menhir_state) * _menhir_state * 'tv_term_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState21 in
            ((Printf.fprintf Pervasives.stderr "State 22:\n%!";
            _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21) : 'freshtv146)) : 'freshtv148)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv153 * _menhir_state) * _menhir_state * 'tv_term_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 29:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv151 * _menhir_state) * _menhir_state * 'tv_term_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | FLOAT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 25\n%!";
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
        | IDENT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (IDENT) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
        | INTEGER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (INTEGER) to state 23\n%!";
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
        | PIPE ->
            Printf.fprintf Pervasives.stderr "Shifting (PIPE) to state 1\n%!";
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | RCURLY ->
            Printf.fprintf Pervasives.stderr "Shifting (RCURLY) to state 30\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv149 * _menhir_state) * _menhir_state * 'tv_term_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState29 in
            ((Printf.fprintf Pervasives.stderr "State 30:\n%!";
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv152)) : 'freshtv154)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv159 * _menhir_state) * _menhir_state * 'tv_term_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 35:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv157 * _menhir_state) * _menhir_state * 'tv_term_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | FLOAT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 25\n%!";
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | IDENT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (IDENT) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | INTEGER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (INTEGER) to state 23\n%!";
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | PIPE ->
            Printf.fprintf Pervasives.stderr "Shifting (PIPE) to state 1\n%!";
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | UNDENT ->
            Printf.fprintf Pervasives.stderr "Shifting (UNDENT) to state 36\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv155 * _menhir_state) * _menhir_state * 'tv_term_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState35 in
            ((Printf.fprintf Pervasives.stderr "State 36:\n%!";
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35) : 'freshtv158)) : 'freshtv160)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv165 * _menhir_state) * _menhir_state * 'tv_term_expr) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 41:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv163 * _menhir_state) * _menhir_state * 'tv_term_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | FLOAT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 25\n%!";
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | IDENT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (IDENT) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | INTEGER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (INTEGER) to state 23\n%!";
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | PIPE ->
            Printf.fprintf Pervasives.stderr "Shifting (PIPE) to state 1\n%!";
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | UNDENT ->
            Printf.fprintf Pervasives.stderr "Shifting (UNDENT) to state 42\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv161 * _menhir_state) * _menhir_state * 'tv_term_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState41 in
            ((Printf.fprintf Pervasives.stderr "State 42:\n%!";
            _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41) : 'freshtv164)) : 'freshtv166)
    | _ ->
        _menhir_fail ()

and _menhir_goto_var : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_var -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_var) = _v in
        ((Printf.fprintf Pervasives.stderr "State 9:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (v : 'tv_var) = _v in
        ((Printf.fprintf Pervasives.stderr "Reducing production arg_list -> var \n%!";
        let _v : 'tv_arg_list = 
# 91 "parser.mly"
            ( [v] )
# 543 "parser.ml"
         in
        _menhir_goto_arg_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv136)) : 'freshtv138)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv141 * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_var) = _v in
        ((Printf.fprintf Pervasives.stderr "State 13:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv139 * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (v : 'tv_var) = _v in
        ((Printf.fprintf Pervasives.stderr "Reducing production arg_list -> arg_list COMMA var \n%!";
        let (_menhir_stack, _menhir_s, a) = _menhir_stack in
        let _v : 'tv_arg_list = 
# 90 "parser.mly"
                                  ( a @ [v] )
# 561 "parser.ml"
         in
        _menhir_goto_arg_list _menhir_env _menhir_stack _menhir_s _v) : 'freshtv140)) : 'freshtv142)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_sep_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_sep_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv111 * _menhir_state * 'tv_sep) * _menhir_state * 'tv_list_sep_) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 18:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv109 * _menhir_state * 'tv_sep) * _menhir_state * 'tv_list_sep_) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "Reducing production list(sep) -> sep list(sep) \n%!";
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_list_sep_ = 
# 116 "/home/kate/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 582 "parser.ml"
         in
        _menhir_goto_list_sep_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv110)) : 'freshtv112)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv125 * _menhir_state * 'tv_var_bind) * _menhir_state * 'tv_list_sep_) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 19:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv123 * _menhir_state * 'tv_var_bind) * _menhir_state * 'tv_list_sep_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | FLOAT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 44\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv113) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState19 in
            let (_v : (
# 52 "parser.mly"
       (float)
# 603 "parser.ml"
            )) = _v in
            ((Printf.fprintf Pervasives.stderr "State 44:\n%!";
            _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v) : 'freshtv114)
        | IDENT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (IDENT) to state 43\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv115) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState19 in
            let (_v : (
# 55 "parser.mly"
       (string)
# 615 "parser.ml"
            )) = _v in
            ((Printf.fprintf Pervasives.stderr "State 43:\n%!";
            _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v) : 'freshtv116)
        | INDENT ->
            Printf.fprintf Pervasives.stderr "Shifting (INDENT) to state 40\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv117) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState19 in
            ((Printf.fprintf Pervasives.stderr "State 40:\n%!";
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack) MenhirState40) : 'freshtv118)
        | INTEGER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (INTEGER) to state 39\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv119) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState19 in
            let (_v : (
# 51 "parser.mly"
       (int)
# 636 "parser.ml"
            )) = _v in
            ((Printf.fprintf Pervasives.stderr "State 39:\n%!";
            _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v) : 'freshtv120)
        | LCURLY ->
            Printf.fprintf Pervasives.stderr "Shifting (LCURLY) to state 20\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv121) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState19 in
            ((Printf.fprintf Pervasives.stderr "State 20:\n%!";
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack) MenhirState20) : 'freshtv122)
        | PIPE ->
            Printf.fprintf Pervasives.stderr "Shifting (PIPE) to state 1\n%!";
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19) : 'freshtv124)) : 'freshtv126)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv133 * _menhir_state * 'tv_var_bind) * _menhir_state * 'tv_list_sep_) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 27:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv131 * _menhir_state * 'tv_var_bind) * _menhir_state * 'tv_list_sep_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | FLOAT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 25\n%!";
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | IDENT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (IDENT) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | INDENT ->
            Printf.fprintf Pervasives.stderr "Shifting (INDENT) to state 34\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv127) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState27 in
            ((Printf.fprintf Pervasives.stderr "State 34:\n%!";
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack) MenhirState34) : 'freshtv128)
        | INTEGER _v ->
            Printf.fprintf Pervasives.stderr "Shifting (INTEGER) to state 23\n%!";
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | LCURLY ->
            Printf.fprintf Pervasives.stderr "Shifting (LCURLY) to state 28\n%!";
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv129) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState27 in
            ((Printf.fprintf Pervasives.stderr "State 28:\n%!";
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack) MenhirState28) : 'freshtv130)
        | PIPE ->
            Printf.fprintf Pervasives.stderr "Shifting (PIPE) to state 1\n%!";
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27) : 'freshtv132)) : 'freshtv134)
    | _ ->
        _menhir_fail ()

and _menhir_goto_sep : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_sep -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState26 | MenhirState17 | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103 * _menhir_state * 'tv_sep) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 17:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * _menhir_state * 'tv_sep) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | NEWLINE ->
            Printf.fprintf Pervasives.stderr "Shifting (NEWLINE) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | SEMICOLON ->
            Printf.fprintf Pervasives.stderr "Shifting (SEMICOLON) to state 15\n%!";
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | FLOAT _ | IDENT _ | INDENT | INTEGER _ | LCURLY | PIPE ->
            _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17) : 'freshtv102)) : 'freshtv104)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv107 * _menhir_state * 'tv_term_expr) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_sep) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 33:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv105 * _menhir_state * 'tv_term_expr) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_sep) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "Reducing production term_expr -> term_expr expr sep \n%!";
        let (((_menhir_stack, _menhir_s, te), _, e), _, _) = _menhir_stack in
        let _v : 'tv_term_expr = 
# 127 "parser.mly"
                                  ( te@[e] )
# 743 "parser.ml"
         in
        _menhir_goto_term_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv106)) : 'freshtv108)
    | _ ->
        _menhir_fail ()

and _menhir_goto_type_annot : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_type_annot -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state * (
# 55 "parser.mly"
       (string)
# 757 "parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_type_annot) = _v in
        ((Printf.fprintf Pervasives.stderr "State 5:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93 * _menhir_state * (
# 55 "parser.mly"
       (string)
# 766 "parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (t : 'tv_type_annot) = _v in
        ((Printf.fprintf Pervasives.stderr "Reducing production var -> IDENT type_annot \n%!";
        let (_menhir_stack, _menhir_s, i) = _menhir_stack in
        let _v : 'tv_var = 
# 118 "parser.mly"
                              ( Plain_Var(i, t) )
# 775 "parser.ml"
         in
        _menhir_goto_var _menhir_env _menhir_stack _menhir_s _v) : 'freshtv94)) : 'freshtv96)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state) * (
# 55 "parser.mly"
       (string)
# 783 "parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_type_annot) = _v in
        ((Printf.fprintf Pervasives.stderr "State 8:\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv97 * _menhir_state) * (
# 55 "parser.mly"
       (string)
# 792 "parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (t : 'tv_type_annot) = _v in
        ((Printf.fprintf Pervasives.stderr "Reducing production var -> BANG IDENT type_annot \n%!";
        let ((_menhir_stack, _menhir_s), i) = _menhir_stack in
        let _v : 'tv_var = 
# 117 "parser.mly"
                                    ( Ref_Var(i, t) )
# 801 "parser.ml"
         in
        _menhir_goto_var _menhir_env _menhir_stack _menhir_s _v) : 'freshtv98)) : 'freshtv100)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "Reducing production list(sep) -> \n%!";
    let _v : 'tv_list_sep_ = 
# 114 "/home/kate/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 818 "parser.ml"
     in
    _menhir_goto_list_sep_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 15:\n%!";
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv91) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((Printf.fprintf Pervasives.stderr "Reducing production sep -> SEMICOLON \n%!";
    let _v : 'tv_sep = 
# 138 "parser.mly"
              ( )
# 833 "parser.ml"
     in
    _menhir_goto_sep _menhir_env _menhir_stack _menhir_s _v) : 'freshtv92)

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 16:\n%!";
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv89) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((Printf.fprintf Pervasives.stderr "Reducing production sep -> NEWLINE \n%!";
    let _v : 'tv_sep = 
# 137 "parser.mly"
            ( )
# 848 "parser.ml"
     in
    _menhir_goto_sep _menhir_env _menhir_stack _menhir_s _v) : 'freshtv90)

and _menhir_reduce19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "Reducing production type_annot -> \n%!";
    let _v : 'tv_type_annot = 
# 122 "parser.mly"
                ( Unknown_Type )
# 858 "parser.ml"
     in
    _menhir_goto_type_annot _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 3:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv87 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | TYPE _v ->
        Printf.fprintf Pervasives.stderr "Shifting (TYPE) to state 4\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 54 "parser.mly"
       (string)
# 878 "parser.ml"
        )) = _v in
        ((Printf.fprintf Pervasives.stderr "State 4:\n%!";
        let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81 * _menhir_state) = Obj.magic _menhir_stack in
        let (t : (
# 54 "parser.mly"
       (string)
# 887 "parser.ml"
        )) = _v in
        ((Printf.fprintf Pervasives.stderr "Reducing production type_annot -> COLON TYPE \n%!";
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : 'tv_type_annot = 
# 123 "parser.mly"
                    ( Known_Type(t) )
# 894 "parser.ml"
         in
        _menhir_goto_type_annot _menhir_env _menhir_stack _menhir_s _v) : 'freshtv82)) : 'freshtv84)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)) : 'freshtv88)

and _menhir_goto_arg_list : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_arg_list -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv79 * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
    ((Printf.fprintf Pervasives.stderr "State 10:\n%!";
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv77 * _menhir_state) * _menhir_state * 'tv_arg_list) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | COMMA ->
        Printf.fprintf Pervasives.stderr "Shifting (COMMA) to state 12\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 12:\n%!";
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * _menhir_state * 'tv_arg_list) = _menhir_stack in
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12) : 'freshtv58)) : 'freshtv60)
    | PIPE ->
        Printf.fprintf Pervasives.stderr "Shifting (PIPE) to state 11\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "State 11:\n%!";
        let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((Printf.fprintf Pervasives.stderr "Reducing production var_bind -> PIPE arg_list PIPE \n%!";
        let ((_menhir_stack, _menhir_s), _, a) = _menhir_stack in
        let _v : 'tv_var_bind = 
# 86 "parser.mly"
                             ( let (a : variable list) = a in a )
# 952 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_var_bind) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState19 | MenhirState0 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv63 * _menhir_state * 'tv_var_bind) = Obj.magic _menhir_stack in
            ((Printf.fprintf Pervasives.stderr "State 14:\n%!";
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv61 * _menhir_state * 'tv_var_bind) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | NEWLINE ->
                Printf.fprintf Pervasives.stderr "Shifting (NEWLINE) to state 16\n%!";
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | SEMICOLON ->
                Printf.fprintf Pervasives.stderr "Shifting (SEMICOLON) to state 15\n%!";
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | FLOAT _ | IDENT _ | INDENT | INTEGER _ | LCURLY | PIPE ->
                _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14) : 'freshtv62)) : 'freshtv64)
        | MenhirState41 | MenhirState27 | MenhirState35 | MenhirState29 | MenhirState21 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv67 * _menhir_state * 'tv_var_bind) = Obj.magic _menhir_stack in
            ((Printf.fprintf Pervasives.stderr "State 26:\n%!";
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv65 * _menhir_state * 'tv_var_bind) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | NEWLINE ->
                Printf.fprintf Pervasives.stderr "Shifting (NEWLINE) to state 16\n%!";
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | SEMICOLON ->
                Printf.fprintf Pervasives.stderr "Shifting (SEMICOLON) to state 15\n%!";
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | FLOAT _ | IDENT _ | INDENT | INTEGER _ | LCURLY | PIPE ->
                _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26) : 'freshtv66)) : 'freshtv68)
        | _ ->
            _menhir_fail ()) : 'freshtv70)) : 'freshtv72)) : 'freshtv74)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state) * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)) : 'freshtv78)) : 'freshtv80)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 55 "parser.mly"
       (string)
# 1020 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 2:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv55 * _menhir_state * (
# 55 "parser.mly"
       (string)
# 1030 "parser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | COLON ->
        Printf.fprintf Pervasives.stderr "Shifting (COLON) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | COMMA | PIPE ->
        _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2) : 'freshtv56)

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 6:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv53 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (IDENT) to state 7\n%!";
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 55 "parser.mly"
       (string)
# 1061 "parser.ml"
        )) = _v in
        ((Printf.fprintf Pervasives.stderr "State 7:\n%!";
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv47 * _menhir_state) * (
# 55 "parser.mly"
       (string)
# 1070 "parser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COLON ->
            Printf.fprintf Pervasives.stderr "Shifting (COLON) to state 3\n%!";
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | COMMA | PIPE ->
            _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv48)) : 'freshtv50)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)) : 'freshtv54)

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
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv9 * _menhir_state) * _menhir_state * 'tv_term_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv13 * _menhir_state) * _menhir_state * 'tv_term_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv17 * _menhir_state * 'tv_term_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv19 * _menhir_state) * _menhir_state * 'tv_term_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv23 * _menhir_state * 'tv_var_bind) * _menhir_state * 'tv_list_sep_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state * 'tv_var_bind) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv27 * _menhir_state) * _menhir_state * 'tv_term_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv31 * _menhir_state * 'tv_var_bind) * _menhir_state * 'tv_list_sep_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state * 'tv_sep) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35 * _menhir_state * 'tv_var_bind) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37 * _menhir_state * 'tv_arg_list) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv39 * _menhir_state) * (
# 55 "parser.mly"
       (string)
# 1189 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv41 * _menhir_state * (
# 55 "parser.mly"
       (string)
# 1198 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv45) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv46)

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
# 92 "parser.mly"
                ( [ ] )
# 1235 "parser.ml"
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

and stmt : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 70 "parser.mly"
       (Ast.toplevel)
# 1329 "parser.ml"
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



