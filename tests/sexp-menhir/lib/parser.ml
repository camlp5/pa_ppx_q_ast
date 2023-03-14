
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | RPAREN
    | LPAREN
    | EOF
    | DOT
    | ATOM of (
# 30 "parser.mly"
       (string)
# 19 "parser.ml"
  )
  
end

include MenhirBasics

# 4 "parser.mly"
  

open Sexp

let mkloc = Location.mkloc
let mknoloc = Location.mknoloc

let make_loc (startpos, endpos) = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = false;
}

let ghost_loc (startpos, endpos) = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = true;
}


# 47 "parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_parse_sexp) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: parse_sexp. *)

  | MenhirState01 : (('s, _menhir_box_parse_sexp) _menhir_cell1_LPAREN, _menhir_box_parse_sexp) _menhir_state
    (** State 01.
        Stack shape : LPAREN.
        Start symbol: parse_sexp. *)

  | MenhirState05 : (('s, _menhir_box_parse_sexp) _menhir_cell1_sexp, _menhir_box_parse_sexp) _menhir_state
    (** State 05.
        Stack shape : sexp.
        Start symbol: parse_sexp. *)

  | MenhirState06 : ((('s, _menhir_box_parse_sexp) _menhir_cell1_sexp, _menhir_box_parse_sexp) _menhir_cell1_DOT, _menhir_box_parse_sexp) _menhir_state
    (** State 06.
        Stack shape : sexp DOT.
        Start symbol: parse_sexp. *)


and ('s, 'r) _menhir_cell1_sexp = 
  | MenhirCell1_sexp of 's * ('s, 'r) _menhir_state * (Sexp.sexp) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_DOT = 
  | MenhirCell1_DOT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state * Lexing.position

and _menhir_box_parse_sexp = 
  | MenhirBox_parse_sexp of (Sexp.sexp) [@@unboxed]

let _menhir_action_1 =
  fun _1 ->
    (
# 80 "parser.mly"
    ( _1 )
# 88 "parser.ml"
     : (Sexp.sexp))

let _menhir_action_2 =
  fun _endpos__2_ _startpos__1_ ->
    let _endpos = _endpos__2_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 86 "parser.mly"
        ( Nil (make_loc _sloc) )
# 99 "parser.ml"
     : (Sexp.sexp))

let _menhir_action_3 =
  fun l ->
    (
# 94 "parser.mly"
        ( l )
# 107 "parser.ml"
     : (Sexp.sexp))

let _menhir_action_4 =
  fun _endpos_l_ _startpos_l_ l ->
    let _endpos = _endpos_l_ in
    let _symbolstartpos = _startpos_l_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 99 "parser.mly"
    ( Cons (make_loc _sloc, l, Nil (make_loc _sloc)) )
# 118 "parser.ml"
     : (Sexp.sexp))

let _menhir_action_5 =
  fun _endpos_r_ _startpos_l_ l r ->
    let _endpos = _endpos_r_ in
    let _symbolstartpos = _startpos_l_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 101 "parser.mly"
    ( Cons (make_loc _sloc, l, r) )
# 129 "parser.ml"
     : (Sexp.sexp))

let _menhir_action_6 =
  fun _endpos_r_ _startpos_l_ l r ->
    let _endpos = _endpos_r_ in
    let _symbolstartpos = _startpos_l_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 103 "parser.mly"
    ( Cons (make_loc _sloc, l, r) )
# 140 "parser.ml"
     : (Sexp.sexp))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ATOM _ ->
        "ATOM"
    | DOT ->
        "DOT"
    | EOF ->
        "EOF"
    | LPAREN ->
        "LPAREN"
    | RPAREN ->
        "RPAREN"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37-39"]
  
  let rec _menhir_run_09 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_parse_sexp =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let _1 = _v in
          let _v = _menhir_action_1 _1 in
          MenhirBox_parse_sexp _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_parse_sexp) _menhir_state -> _menhir_box_parse_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_endpos__2_, _startpos__1_) = (_endpos, _startpos) in
          let _v = _menhir_action_2 _endpos__2_ _startpos__1_ in
          _menhir_goto_sexp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _startpos__1_ _v _menhir_s _tok
      | LPAREN ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | _ ->
          _eRR ()
  
  and _menhir_goto_sexp : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_parse_sexp) _menhir_state -> _ -> _menhir_box_parse_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_09 _menhir_stack _v _tok
      | MenhirState06 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState05 ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState01 ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
  
  and _menhir_run_07 : type  ttv_stack. ((ttv_stack, _menhir_box_parse_sexp) _menhir_cell1_sexp, _menhir_box_parse_sexp) _menhir_cell1_DOT -> _ -> _ -> _ -> _ -> _ -> _menhir_box_parse_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_DOT (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_sexp (_menhir_stack, _menhir_s, l, _startpos_l_, _) = _menhir_stack in
      let (_endpos_r_, r) = (_endpos, _v) in
      let _v = _menhir_action_6 _endpos_r_ _startpos_l_ l r in
      _menhir_goto_sexp_list _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_r_ _v _menhir_s _tok
  
  and _menhir_goto_sexp_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_parse_sexp) _menhir_state -> _ -> _menhir_box_parse_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState05 ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState01 ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_08 : type  ttv_stack. (ttv_stack, _menhir_box_parse_sexp) _menhir_cell1_sexp -> _ -> _ -> _ -> _ -> _ -> _menhir_box_parse_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_sexp (_menhir_stack, _menhir_s, l, _startpos_l_, _) = _menhir_stack in
      let (_endpos_r_, r) = (_endpos, _v) in
      let _v = _menhir_action_5 _endpos_r_ _startpos_l_ l r in
      _menhir_goto_sexp_list _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_r_ _v _menhir_s _tok
  
  and _menhir_run_03 : type  ttv_stack. (ttv_stack, _menhir_box_parse_sexp) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> _menhir_box_parse_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (l, _endpos__3_) = (_v, _endpos_0) in
          let _v = _menhir_action_3 l in
          _menhir_goto_sexp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_05 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_parse_sexp) _menhir_state -> _ -> _menhir_box_parse_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_stack = MenhirCell1_sexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState05
      | DOT ->
          let _menhir_stack = MenhirCell1_sexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_stack = MenhirCell1_DOT (_menhir_stack, MenhirState05) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState06
          | _ ->
              _eRR ())
      | RPAREN ->
          let (_endpos_l_, _startpos_l_, l) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_4 _endpos_l_ _startpos_l_ l in
          _menhir_goto_sexp_list _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_l_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  let rec _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_parse_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | _ ->
          _eRR ()
  
end

let parse_sexp =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_parse_sexp v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

# 106 "parser.mly"
  

# 285 "parser.ml"