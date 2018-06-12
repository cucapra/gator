
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | TRUE
    | TRANS
    | TIMES
    | TAG
    | SKIP
    | SEMI
    | RPAREN
    | RBRACK
    | RBRACE
    | PRINT
    | PLUS
    | OR
    | NUM of (
# 16 "parser.mly"
       (int)
# 23 "parser.ml"
  )
    | NOT
    | NORM
    | MINUS
    | LPAREN
    | LEQ
    | LBRACK
    | LBRACE
    | IS
    | INTTYP
    | IF
    | ID of (
# 18 "parser.mly"
       (string)
# 38 "parser.ml"
  )
    | GETS
    | FLOATTYP
    | FLOAT of (
# 17 "parser.mly"
       (float)
# 45 "parser.ml"
  )
    | FALSE
    | EQ
    | EOL
    | ELSE
    | DOT
    | CTIMES
    | COMMA
    | COLON
    | BOOLTYP
    | AND
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState93
  | MenhirState90
  | MenhirState87
  | MenhirState81
  | MenhirState80
  | MenhirState75
  | MenhirState73
  | MenhirState72
  | MenhirState68
  | MenhirState67
  | MenhirState64
  | MenhirState62
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState57
  | MenhirState56
  | MenhirState55
  | MenhirState54
  | MenhirState53
  | MenhirState51
  | MenhirState50
  | MenhirState49
  | MenhirState45
  | MenhirState44
  | MenhirState43
  | MenhirState38
  | MenhirState34
  | MenhirState30
  | MenhirState26
  | MenhirState23
  | MenhirState21
  | MenhirState19
  | MenhirState17
  | MenhirState16
  | MenhirState15
  | MenhirState14
  | MenhirState11
  | MenhirState6
  | MenhirState3
  | MenhirState0

# 2 "parser.mly"
  
open Ast
open Str

exception ParseException of string

let matr = Str.regexp "mat\\([0-9]+\\)x\\([0-9]+\\)"
let vec = Str.regexp "vec\\([0-9]+\\)"


# 127 "parser.ml"

let rec _menhir_goto_matlit : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_matlit -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv367 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_matlit) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv365 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_matlit) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _), _, (m2 : 'tv_matlit)) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_matlit = 
# 152 "parser.mly"
                                       ( [[]]@m2@[] )
# 145 "parser.ml"
         in
        _menhir_goto_matlit _menhir_env _menhir_stack _menhir_s _v) : 'freshtv366)) : 'freshtv368)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv371 * _menhir_state) * _menhir_state * 'tv_veclit))) * _menhir_state * 'tv_matlit) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv369 * _menhir_state) * _menhir_state * 'tv_veclit))) * _menhir_state * 'tv_matlit) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (v : 'tv_veclit)), _, (m2 : 'tv_matlit)) = _menhir_stack in
        let _4 = () in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_matlit = 
# 151 "parser.mly"
                                                   ( [v]@m2@[] )
# 160 "parser.ml"
         in
        _menhir_goto_matlit _menhir_env _menhir_stack _menhir_s _v) : 'freshtv370)) : 'freshtv372)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv381 * _menhir_state) * _menhir_state * 'tv_matlit) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv377 * _menhir_state) * _menhir_state * 'tv_matlit) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv373 * _menhir_state) * _menhir_state * 'tv_matlit)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ID _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38) : 'freshtv374)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv375 * _menhir_state) * _menhir_state * 'tv_matlit)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv376)) : 'freshtv378)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv379 * _menhir_state) * _menhir_state * 'tv_matlit) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv380)) : 'freshtv382)
    | _ ->
        _menhir_fail ()

and _menhir_goto_veclit : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_veclit -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv341 * _menhir_state * (
# 17 "parser.mly"
       (float)
# 213 "parser.ml"
        ))) * _menhir_state * 'tv_veclit) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv339 * _menhir_state * (
# 17 "parser.mly"
       (float)
# 219 "parser.ml"
        ))) * _menhir_state * 'tv_veclit) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (f : (
# 17 "parser.mly"
       (float)
# 224 "parser.ml"
        ))), _, (v2 : 'tv_veclit)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_veclit = 
# 145 "parser.mly"
                                  ( f::v2@[] )
# 230 "parser.ml"
         in
        _menhir_goto_veclit _menhir_env _menhir_stack _menhir_s _v) : 'freshtv340)) : 'freshtv342)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv353 * _menhir_state) * _menhir_state * 'tv_veclit) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv349 * _menhir_state) * _menhir_state * 'tv_veclit) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COMMA ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv343 * _menhir_state) * _menhir_state * 'tv_veclit)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LBRACK ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30) : 'freshtv344)
            | RBRACK ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv345 * _menhir_state) * _menhir_state * 'tv_veclit)) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _, (v : 'tv_veclit)) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _v : 'tv_matlit = 
# 149 "parser.mly"
                               ( [v] )
# 266 "parser.ml"
                 in
                _menhir_goto_matlit _menhir_env _menhir_stack _menhir_s _v) : 'freshtv346)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv347 * _menhir_state) * _menhir_state * 'tv_veclit)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv348)) : 'freshtv350)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv351 * _menhir_state) * _menhir_state * 'tv_veclit) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv352)) : 'freshtv354)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv363 * _menhir_state) * _menhir_state * 'tv_veclit) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv359 * _menhir_state) * _menhir_state * 'tv_veclit) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv355 * _menhir_state) * _menhir_state * 'tv_veclit)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ID _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34) : 'freshtv356)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv357 * _menhir_state) * _menhir_state * 'tv_veclit)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv358)) : 'freshtv360)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv361 * _menhir_state) * _menhir_state * 'tv_veclit) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv362)) : 'freshtv364)
    | _ ->
        _menhir_fail ()

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | FLOAT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | ID _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | LBRACK ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NORM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NUM _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | TRUE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run49 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | FLOAT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | ID _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LBRACK ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NORM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NUM _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | TRUE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | FLOAT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | ID _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | LBRACK ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NORM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NUM _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | TRUE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run55 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | FLOAT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | ID _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | LBRACK ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NORM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NUM _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | TRUE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | FLOAT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | ID _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | LBRACK ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NORM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NUM _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | TRUE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | FLOAT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | ID _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | LBRACK ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NORM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NUM _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | TRUE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run51 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | FLOAT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | ID _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | LBRACK ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NORM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NUM _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | TRUE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | FLOAT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | ID _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | LBRACK ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NORM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NUM _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | TRUE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_goto_main : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 73 "parser.mly"
      (Ast.prog)
# 575 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv337) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 73 "parser.mly"
      (Ast.prog)
# 584 "parser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv335) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 73 "parser.mly"
      (Ast.prog)
# 592 "parser.ml"
    )) : (
# 73 "parser.mly"
      (Ast.prog)
# 596 "parser.ml"
    )) = _v in
    (Obj.magic _1 : 'freshtv336)) : 'freshtv338)

and _menhir_goto_commlst : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_commlst -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv305 * _menhir_state)) * _menhir_state * 'tv_exp) * _menhir_state)) * _menhir_state * 'tv_commlst) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv301 * _menhir_state)) * _menhir_state * 'tv_exp) * _menhir_state)) * _menhir_state * 'tv_commlst) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ELSE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv297 * _menhir_state)) * _menhir_state * 'tv_exp) * _menhir_state)) * _menhir_state * 'tv_commlst)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LBRACE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((((((('freshtv293 * _menhir_state)) * _menhir_state * 'tv_exp) * _menhir_state)) * _menhir_state * 'tv_commlst))) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | BOOLTYP ->
                        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | FLOATTYP ->
                        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | ID _v ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
                    | IF ->
                        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | INTTYP ->
                        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | PRINT ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | SKIP ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87) : 'freshtv294)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((((((('freshtv295 * _menhir_state)) * _menhir_state * 'tv_exp) * _menhir_state)) * _menhir_state * 'tv_commlst))) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv296)) : 'freshtv298)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((('freshtv299 * _menhir_state)) * _menhir_state * 'tv_exp) * _menhir_state)) * _menhir_state * 'tv_commlst)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv300)) : 'freshtv302)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv303 * _menhir_state)) * _menhir_state * 'tv_exp) * _menhir_state)) * _menhir_state * 'tv_commlst) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv304)) : 'freshtv306)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((('freshtv313 * _menhir_state)) * _menhir_state * 'tv_exp) * _menhir_state)) * _menhir_state * 'tv_commlst)))) * _menhir_state * 'tv_commlst) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((('freshtv309 * _menhir_state)) * _menhir_state * 'tv_exp) * _menhir_state)) * _menhir_state * 'tv_commlst)))) * _menhir_state * 'tv_commlst) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((('freshtv307 * _menhir_state)) * _menhir_state * 'tv_exp) * _menhir_state)) * _menhir_state * 'tv_commlst)))) * _menhir_state * 'tv_commlst) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s), _, (b1 : 'tv_exp)), _), _, (c1 : 'tv_commlst)), _, (c2 : 'tv_commlst)) = _menhir_stack in
            let _11 = () in
            let _9 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_comm = 
# 106 "parser.mly"
                                       ( If(b1,c1,c2) )
# 691 "parser.ml"
             in
            _menhir_goto_comm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv308)) : 'freshtv310)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((('freshtv311 * _menhir_state)) * _menhir_state * 'tv_exp) * _menhir_state)) * _menhir_state * 'tv_commlst)))) * _menhir_state * 'tv_commlst) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv312)) : 'freshtv314)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv317 * _menhir_state * 'tv_comm) * _menhir_state * 'tv_commlst) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv315 * _menhir_state * 'tv_comm) * _menhir_state * 'tv_commlst) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (c1 : 'tv_comm)), _, (c2 : 'tv_commlst)) = _menhir_stack in
        let _v : 'tv_commlst = 
# 97 "parser.mly"
                            ( c1::c2@[] )
# 710 "parser.ml"
         in
        _menhir_goto_commlst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv316)) : 'freshtv318)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv325 * _menhir_state * 'tv_taglst) * _menhir_state * 'tv_commlst) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv321 * _menhir_state * 'tv_taglst) * _menhir_state * 'tv_commlst) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv319 * _menhir_state * 'tv_taglst) * _menhir_state * 'tv_commlst) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (t : 'tv_taglst)), _, (e : 'tv_commlst)) = _menhir_stack in
            let _3 = () in
            let _v : (
# 73 "parser.mly"
      (Ast.prog)
# 729 "parser.ml"
            ) = 
# 81 "parser.mly"
                                 ( Prog(t, e) )
# 733 "parser.ml"
             in
            _menhir_goto_main _menhir_env _menhir_stack _menhir_s _v) : 'freshtv320)) : 'freshtv322)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv323 * _menhir_state * 'tv_taglst) * _menhir_state * 'tv_commlst) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv324)) : 'freshtv326)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv333 * _menhir_state * 'tv_commlst) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv329 * _menhir_state * 'tv_commlst) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv327 * _menhir_state * 'tv_commlst) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (e : 'tv_commlst)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 73 "parser.mly"
      (Ast.prog)
# 759 "parser.ml"
            ) = 
# 82 "parser.mly"
                     (Prog([], e))
# 763 "parser.ml"
             in
            _menhir_goto_main _menhir_env _menhir_stack _menhir_s _v) : 'freshtv328)) : 'freshtv330)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv331 * _menhir_state * 'tv_commlst) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv332)) : 'freshtv334)
    | _ ->
        _menhir_fail ()

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | RBRACK ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv291 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState21 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv285 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LBRACK ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23) : 'freshtv286)
        | RBRACK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv287 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_matlit = 
# 150 "parser.mly"
                   ( [[]] )
# 813 "parser.ml"
             in
            _menhir_goto_matlit _menhir_env _menhir_stack _menhir_s _v) : 'freshtv288)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv289 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv290)) : 'freshtv292)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 17 "parser.mly"
       (float)
# 831 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv279 * _menhir_state * (
# 17 "parser.mly"
       (float)
# 843 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FLOAT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26) : 'freshtv280)
    | RBRACK ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv281 * _menhir_state * (
# 17 "parser.mly"
       (float)
# 859 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (f : (
# 17 "parser.mly"
       (float)
# 864 "parser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_veclit = 
# 144 "parser.mly"
              ( f::[] )
# 869 "parser.ml"
         in
        _menhir_goto_veclit _menhir_env _menhir_stack _menhir_s _v) : 'freshtv282)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv283 * _menhir_state * (
# 17 "parser.mly"
       (float)
# 879 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv284)

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv207 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | CTIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | DOT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | EQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | FALSE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | FLOAT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | ID _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | LBRACK ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | LPAREN ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | MINUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | NORM ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | NOT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | NUM _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | OR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | TRUE ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44) : 'freshtv208)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv211 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv209 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_exp)), _), _, (e2 : 'tv_exp)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_exp = 
# 178 "parser.mly"
                              ( Times(e1,e2) )
# 944 "parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv210)) : 'freshtv212)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv215 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CTIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | AND | DOT | EQ | FALSE | FLOAT _ | ID _ | LBRACK | LEQ | LPAREN | MINUS | NORM | NOT | NUM _ | OR | PLUS | RPAREN | SEMI | TRUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv213 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_exp)), _), _, (e2 : 'tv_exp)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_exp = 
# 177 "parser.mly"
                             ( Plus(e1,e2) )
# 965 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv214)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv216)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv219 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv217 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_exp)), _), _, (e2 : 'tv_exp)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_exp = 
# 180 "parser.mly"
                               ( CTimes(e1,e2) )
# 982 "parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv218)) : 'freshtv220)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv223 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CTIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | EQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | MINUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | AND | DOT | FALSE | FLOAT _ | ID _ | LBRACK | LPAREN | NORM | NOT | NUM _ | OR | RPAREN | SEMI | TRUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv221 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_exp)), _), _, (e2 : 'tv_exp)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_exp = 
# 184 "parser.mly"
                           ( Or(e1,e2) )
# 1011 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv222)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54) : 'freshtv224)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv227 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CTIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | AND | DOT | EQ | FALSE | FLOAT _ | ID _ | LBRACK | LEQ | LPAREN | MINUS | NORM | NOT | NUM _ | OR | PLUS | RPAREN | SEMI | TRUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv225 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_exp)), _), _, (e2 : 'tv_exp)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_exp = 
# 179 "parser.mly"
                              ( Minus(e1,e2) )
# 1036 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv226)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56) : 'freshtv228)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv231 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CTIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | MINUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | AND | DOT | EQ | FALSE | FLOAT _ | ID _ | LBRACK | LEQ | LPAREN | NORM | NOT | NUM _ | OR | RPAREN | SEMI | TRUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv229 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_exp)), _), _, (e2 : 'tv_exp)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_exp = 
# 183 "parser.mly"
                            ( Leq(e1,e2) )
# 1065 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv230)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58) : 'freshtv232)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv235 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CTIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | MINUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | AND | DOT | EQ | FALSE | FLOAT _ | ID _ | LBRACK | LEQ | LPAREN | NORM | NOT | NUM _ | OR | RPAREN | SEMI | TRUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv233 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_exp)), _), _, (e2 : 'tv_exp)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_exp = 
# 182 "parser.mly"
                           ( Eq(e1,e2) )
# 1094 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv234)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60) : 'freshtv236)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv239 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CTIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | EQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | MINUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | AND | DOT | FALSE | FLOAT _ | ID _ | LBRACK | LPAREN | NORM | NOT | NUM _ | OR | RPAREN | SEMI | TRUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv237 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e1 : 'tv_exp)), _), _, (e2 : 'tv_exp)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_exp = 
# 185 "parser.mly"
                            ( And(e1,e2) )
# 1127 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv238)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62) : 'freshtv240)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv243 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv241 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (e1 : 'tv_exp)), _, (e2 : 'tv_exp)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_exp = 
# 175 "parser.mly"
                            ( Dot(e1, e2) )
# 1144 "parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv242)) : 'freshtv244)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv249 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | CTIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | EQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MINUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | OR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv247 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState64 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv245 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (a : 'tv_exp)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_exp = 
# 161 "parser.mly"
                            ( a )
# 1181 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv246)) : 'freshtv248)
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64) : 'freshtv250)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv253 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv251 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_exp)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_exp = 
# 176 "parser.mly"
                  ( Norm(e) )
# 1200 "parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv252)) : 'freshtv254)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv257 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CTIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | MINUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | AND | DOT | EQ | FALSE | FLOAT _ | ID _ | LBRACK | LEQ | LPAREN | NORM | NOT | NUM _ | OR | RPAREN | SEMI | TRUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv255 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e1 : 'tv_exp)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_exp = 
# 181 "parser.mly"
                  ( Not(e1) )
# 1225 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv256)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67) : 'freshtv258)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv263 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | CTIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | EQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | MINUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | OR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv261 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState68 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv259 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_exp)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_comm = 
# 107 "parser.mly"
                          ( Print(e) )
# 1266 "parser.ml"
             in
            _menhir_goto_comm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv260)) : 'freshtv262)
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68) : 'freshtv264)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv271 * _menhir_state)) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | CTIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | EQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MINUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | OR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv269 * _menhir_state)) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState73 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LBRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv265 * _menhir_state)) * _menhir_state * 'tv_exp) * _menhir_state) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | BOOLTYP ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | FLOATTYP ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | ID _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
                | IF ->
                    _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | INTTYP ->
                    _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | PRINT ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | SKIP ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75) : 'freshtv266)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv267 * _menhir_state)) * _menhir_state * 'tv_exp) * _menhir_state) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv268)) : 'freshtv270)
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73) : 'freshtv272)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv277 * _menhir_state * 'tv_typ) * (
# 18 "parser.mly"
       (string)
# 1345 "parser.ml"
        ))) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | CTIMES ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | EQ ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MINUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | OR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | PLUS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv275 * _menhir_state * 'tv_typ) * (
# 18 "parser.mly"
       (string)
# 1369 "parser.ml"
            ))) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState81 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv273 * _menhir_state * 'tv_typ) * (
# 18 "parser.mly"
       (string)
# 1377 "parser.ml"
            ))) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _menhir_s, (t : 'tv_typ)), (x : (
# 18 "parser.mly"
       (string)
# 1383 "parser.ml"
            ))), _, (e1 : 'tv_exp)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _v : 'tv_comm = 
# 102 "parser.mly"
                                           (  if (Str.string_match matr x 0) || (Str.string_match vec x 0) then (
      raise (ParseException "invalid id specified for variable declaration")
    ) else Decl(t, x, e1) )
# 1392 "parser.ml"
             in
            _menhir_goto_comm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv274)) : 'freshtv276)
        | TIMES ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv278)
    | _ ->
        _menhir_fail ()

and _menhir_goto_bool : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_bool -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv205) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_bool) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv203) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((b : 'tv_bool) : 'tv_bool) = _v in
    ((let _v : 'tv_exp = 
# 163 "parser.mly"
             ( Bool b )
# 1417 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv204)) : 'freshtv206)

and _menhir_goto_aval : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_aval -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv201) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_aval) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv199) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((a : 'tv_aval) : 'tv_aval) = _v in
    ((let _v : 'tv_exp = 
# 162 "parser.mly"
             ( Aval a )
# 1434 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv200)) : 'freshtv202)

and _menhir_run6 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_ltyp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_taglst : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_taglst -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv197 * _menhir_state * 'tv_taglst) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYP ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | EOL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv195 * _menhir_state * 'tv_taglst) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState93 in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv193 * _menhir_state * 'tv_taglst) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s, (t : 'tv_taglst)) = _menhir_stack in
        let _2 = () in
        let _v : (
# 73 "parser.mly"
      (Ast.prog)
# 1477 "parser.ml"
        ) = 
# 83 "parser.mly"
                    (Prog(t, []))
# 1481 "parser.ml"
         in
        _menhir_goto_main _menhir_env _menhir_stack _menhir_s _v) : 'freshtv194)) : 'freshtv196)
    | FLOATTYP ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | ID _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | IF ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | INTTYP ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | PRINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | SKIP ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | TAG ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93) : 'freshtv198)

and _menhir_goto_comm : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_comm -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv191 * _menhir_state * 'tv_comm) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYP ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | FLOATTYP ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | ID _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | IF ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | INTTYP ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | PRINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | SKIP ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | EOL | RBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv189 * _menhir_state * 'tv_comm) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (c : 'tv_comm)) = _menhir_stack in
        let _v : 'tv_commlst = 
# 96 "parser.mly"
             ( c::[] )
# 1532 "parser.ml"
         in
        _menhir_goto_commlst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv190)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90) : 'freshtv192)

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv187) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_bool = 
# 156 "parser.mly"
         ( true )
# 1550 "parser.ml"
     in
    _menhir_goto_bool _menhir_env _menhir_stack _menhir_s _v) : 'freshtv188)

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 16 "parser.mly"
       (int)
# 1557 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv185) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 16 "parser.mly"
       (int)
# 1567 "parser.ml"
    )) : (
# 16 "parser.mly"
       (int)
# 1571 "parser.ml"
    )) = _v in
    ((let _v : 'tv_aval = 
# 136 "parser.mly"
            ( Num i )
# 1576 "parser.ml"
     in
    _menhir_goto_aval _menhir_env _menhir_stack _menhir_s _v) : 'freshtv186)

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | FLOAT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | ID _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | LBRACK ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | NORM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | NUM _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | TRUE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | FLOAT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | ID _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | LBRACK ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | NORM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | NUM _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | TRUE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | FLOAT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | ID _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | LBRACK ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | NORM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | NUM _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | TRUE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | LBRACK ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | RBRACK ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv183 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState17 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv179 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19) : 'freshtv180)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv181 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)) : 'freshtv184)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 18 "parser.mly"
       (string)
# 1718 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv177) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((x : (
# 18 "parser.mly"
       (string)
# 1728 "parser.ml"
    )) : (
# 18 "parser.mly"
       (string)
# 1732 "parser.ml"
    )) = _v in
    ((let _v : 'tv_exp = 
# 164 "parser.mly"
           ( if (Str.string_match matr x 0) then (
              let len = String.length x in 
              let dim = String.sub x 3 (len-3) in
              let dim_lst = Str.split_delim (regexp "x") dim in
              Typ(ATyp(LTyp(MatTyp (int_of_string(List.nth dim_lst 0),int_of_string(List.nth dim_lst 1)))))
            ) else if (Str.string_match vec x 0) then (
              let len = String.length x in 
              let dim = String.sub x 3 (len-3)in
              Typ(ATyp(LTyp(VecTyp (int_of_string(dim)))))
            ) else Var x
           )
# 1747 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv178)

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 17 "parser.mly"
       (float)
# 1754 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv175) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((f : (
# 17 "parser.mly"
       (float)
# 1764 "parser.ml"
    )) : (
# 17 "parser.mly"
       (float)
# 1768 "parser.ml"
    )) = _v in
    ((let _v : 'tv_aval = 
# 137 "parser.mly"
              ( Float f )
# 1773 "parser.ml"
     in
    _menhir_goto_aval _menhir_env _menhir_stack _menhir_s _v) : 'freshtv176)

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv173) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_bool = 
# 157 "parser.mly"
          ( false )
# 1787 "parser.ml"
     in
    _menhir_goto_bool _menhir_env _menhir_stack _menhir_s _v) : 'freshtv174)

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | FLOAT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | ID _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | LBRACK ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NORM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NUM _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | TRUE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_goto_ltyp : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_ltyp -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv143 * _menhir_state) * (
# 18 "parser.mly"
       (string)
# 1831 "parser.ml"
        ))) * _menhir_state * 'tv_ltyp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMI ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv139 * _menhir_state) * (
# 18 "parser.mly"
       (string)
# 1841 "parser.ml"
            ))) * _menhir_state * 'tv_ltyp) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv137 * _menhir_state) * (
# 18 "parser.mly"
       (string)
# 1848 "parser.ml"
            ))) * _menhir_state * 'tv_ltyp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), (x : (
# 18 "parser.mly"
       (string)
# 1853 "parser.ml"
            ))), _, (e1 : 'tv_ltyp)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_tag = 
# 92 "parser.mly"
                                      ( TagDecl(x, LTyp(e1)) )
# 1861 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv135) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_tag) = _v in
            ((match _menhir_s with
            | MenhirState93 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv129 * _menhir_state * 'tv_taglst) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_tag) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv127 * _menhir_state * 'tv_taglst) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let ((t2 : 'tv_tag) : 'tv_tag) = _v in
                ((let (_menhir_stack, _menhir_s, (t1 : 'tv_taglst)) = _menhir_stack in
                let _v : 'tv_taglst = 
# 88 "parser.mly"
                          ( t1@(t2::[])@[] )
# 1881 "parser.ml"
                 in
                _menhir_goto_taglst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv128)) : 'freshtv130)
            | MenhirState0 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv133) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_tag) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv131) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((t : 'tv_tag) : 'tv_tag) = _v in
                ((let _v : 'tv_taglst = 
# 87 "parser.mly"
            ( t::[] )
# 1896 "parser.ml"
                 in
                _menhir_goto_taglst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv132)) : 'freshtv134)
            | _ ->
                _menhir_fail ()) : 'freshtv136)) : 'freshtv138)) : 'freshtv140)
        | TRANS ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv141 * _menhir_state) * (
# 18 "parser.mly"
       (string)
# 1910 "parser.ml"
            ))) * _menhir_state * 'tv_ltyp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)) : 'freshtv144)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv147 * _menhir_state * 'tv_ltyp)) * _menhir_state * 'tv_ltyp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv145 * _menhir_state * 'tv_ltyp)) * _menhir_state * 'tv_ltyp) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x1 : 'tv_ltyp)), _, (x2 : 'tv_ltyp)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_ltyp = 
# 132 "parser.mly"
                                ( TransTyp(x1,x2) )
# 1924 "parser.ml"
         in
        _menhir_goto_ltyp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv146)) : 'freshtv148)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv153 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_ltyp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TRANS ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
        | AND | CTIMES | DOT | EQ | FALSE | FLOAT _ | ID _ | LBRACK | LEQ | LPAREN | MINUS | NORM | NOT | NUM _ | OR | PLUS | RPAREN | SEMI | TIMES | TRUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv149 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_ltyp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _), _, (t : 'tv_ltyp)) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_aval = 
# 138 "parser.mly"
                                   (VecLit([], t))
# 1945 "parser.ml"
             in
            _menhir_goto_aval _menhir_env _menhir_stack _menhir_s _v) : 'freshtv150)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv151 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_ltyp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)) : 'freshtv154)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv159 * _menhir_state) * _menhir_state * 'tv_veclit))) * _menhir_state * 'tv_ltyp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TRANS ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
        | AND | CTIMES | DOT | EQ | FALSE | FLOAT _ | ID _ | LBRACK | LEQ | LPAREN | MINUS | NORM | NOT | NUM _ | OR | PLUS | RPAREN | SEMI | TIMES | TRUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv155 * _menhir_state) * _menhir_state * 'tv_veclit))) * _menhir_state * 'tv_ltyp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (v : 'tv_veclit)), _, (t : 'tv_ltyp)) = _menhir_stack in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_aval = 
# 139 "parser.mly"
                                               ( VecLit(v@[], t) )
# 1973 "parser.ml"
             in
            _menhir_goto_aval _menhir_env _menhir_stack _menhir_s _v) : 'freshtv156)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv157 * _menhir_state) * _menhir_state * 'tv_veclit))) * _menhir_state * 'tv_ltyp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)) : 'freshtv160)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv165 * _menhir_state) * _menhir_state * 'tv_matlit))) * _menhir_state * 'tv_ltyp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TRANS ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
        | AND | CTIMES | DOT | EQ | FALSE | FLOAT _ | ID _ | LBRACK | LEQ | LPAREN | MINUS | NORM | NOT | NUM _ | OR | PLUS | RPAREN | SEMI | TIMES | TRUE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv161 * _menhir_state) * _menhir_state * 'tv_matlit))) * _menhir_state * 'tv_ltyp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (m : 'tv_matlit)), _, (t : 'tv_ltyp)) = _menhir_stack in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_aval = 
# 140 "parser.mly"
                                                ( MatLit(m@[], t) )
# 2001 "parser.ml"
             in
            _menhir_goto_aval _menhir_env _menhir_stack _menhir_s _v) : 'freshtv162)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv163 * _menhir_state) * _menhir_state * 'tv_matlit))) * _menhir_state * 'tv_ltyp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)) : 'freshtv166)
    | MenhirState0 | MenhirState93 | MenhirState90 | MenhirState87 | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv171 * _menhir_state * 'tv_ltyp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | TRANS ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack)
        | ID _ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv167 * _menhir_state * 'tv_ltyp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (e : 'tv_ltyp)) = _menhir_stack in
            let _v : 'tv_atyp = 
# 118 "parser.mly"
             ( LTyp(e) )
# 2026 "parser.ml"
             in
            _menhir_goto_atyp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv168)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv169 * _menhir_state * 'tv_ltyp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)) : 'freshtv172)
    | _ ->
        _menhir_fail ()

and _menhir_goto_atyp : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_atyp -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv125) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_atyp) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv123) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((a : 'tv_atyp) : 'tv_atyp) = _v in
    ((let _v : 'tv_typ = 
# 111 "parser.mly"
             ( ATyp(a) )
# 2052 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv124)) : 'freshtv126)

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_typ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv121 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        let (_v : (
# 18 "parser.mly"
       (string)
# 2070 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | GETS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv113 * _menhir_state * 'tv_typ) * (
# 18 "parser.mly"
       (string)
# 2081 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DOT ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | FALSE ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | FLOAT _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | ID _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | LBRACK ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | LPAREN ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | NORM ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | NOT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | NUM _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | TRUE ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80) : 'freshtv114)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv115 * _menhir_state * 'tv_typ) * (
# 18 "parser.mly"
       (string)
# 2117 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)) : 'freshtv118)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)) : 'freshtv122)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * _menhir_state * 'tv_taglst) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state * 'tv_comm) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv33 * _menhir_state)) * _menhir_state * 'tv_exp) * _menhir_state)) * _menhir_state * 'tv_commlst)))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv35 * _menhir_state * 'tv_typ) * (
# 18 "parser.mly"
       (string)
# 2152 "parser.ml"
        ))) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv37 * _menhir_state * 'tv_typ) * (
# 18 "parser.mly"
       (string)
# 2161 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv39 * _menhir_state)) * _menhir_state * 'tv_exp) * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv41 * _menhir_state)) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv45 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv47 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv49 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv51 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv53 * _menhir_state * 'tv_exp) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv55 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv57 * _menhir_state * 'tv_exp) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv59 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * 'tv_exp) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv63 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv65 * _menhir_state * 'tv_exp) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv67 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv69 * _menhir_state * 'tv_exp) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * 'tv_exp) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv73 * _menhir_state * 'tv_exp) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state * 'tv_exp) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state * 'tv_exp) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv83 * _menhir_state) * _menhir_state * 'tv_matlit))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv85 * _menhir_state) * _menhir_state * 'tv_veclit))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv87 * _menhir_state) * _menhir_state * 'tv_veclit))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state * (
# 17 "parser.mly"
       (float)
# 2295 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv91 * _menhir_state) * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv95 * _menhir_state) * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv105 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv107 * _menhir_state * 'tv_ltyp)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv109 * _menhir_state) * (
# 18 "parser.mly"
       (string)
# 2349 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv112)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 18 "parser.mly"
       (string)
# 2370 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv21 * _menhir_state) * (
# 18 "parser.mly"
       (string)
# 2381 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ID _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3) : 'freshtv22)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv23 * _menhir_state) * (
# 18 "parser.mly"
       (string)
# 2399 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)) : 'freshtv26)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_comm = 
# 101 "parser.mly"
               ( Skip )
# 2429 "parser.ml"
         in
        _menhir_goto_comm _menhir_env _menhir_stack _menhir_s _v) : 'freshtv16)) : 'freshtv18)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | FLOAT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | ID _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | LBRACK ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | LPAREN ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | NORM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | NOT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | NUM _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | TRUE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv13) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_atyp = 
# 117 "parser.mly"
              ( IntTyp )
# 2481 "parser.ml"
     in
    _menhir_goto_atyp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv14)

and _menhir_run71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | FALSE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | FLOAT _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | ID _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | LBRACK ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | LPAREN ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | NORM ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | NOT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | NUM _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | TRUE ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72) : 'freshtv10)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 18 "parser.mly"
       (string)
# 2532 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((x : (
# 18 "parser.mly"
       (string)
# 2542 "parser.ml"
    )) : (
# 18 "parser.mly"
       (string)
# 2546 "parser.ml"
    )) = _v in
    ((let _v : 'tv_ltyp = 
# 122 "parser.mly"
           ( if (Str.string_match matr x 0) then (
              let len = String.length x in 
              let dim = String.sub x 3 (len-3) in
              let dim_lst = Str.split_delim (regexp "x") dim in
              Printf.printf "%s" (List.nth dim_lst 0) ;MatTyp (int_of_string(List.nth dim_lst 0),int_of_string(List.nth dim_lst 1))
            ) else if (Str.string_match vec x 0) then (
              let len = String.length x in 
              let dim = String.sub x 3 (len-3)in
              VecTyp (int_of_string(dim))
            ) else TagTyp(x) )
# 2560 "parser.ml"
     in
    _menhir_goto_ltyp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv8)

and _menhir_run76 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_atyp = 
# 116 "parser.mly"
              ( FloatTyp )
# 2574 "parser.ml"
     in
    _menhir_goto_atyp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv6)

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_typ = 
# 112 "parser.mly"
            ( BTyp )
# 2588 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv4)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 73 "parser.mly"
      (Ast.prog)
# 2607 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYP ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FLOATTYP ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INTTYP ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PRINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SKIP ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TAG ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 188 "parser.mly"
  
# 2649 "parser.ml"

# 219 "/Users/euisuny/.opam/4.06.0/lib/menhir/standard.mly"
  


# 2655 "parser.ml"
