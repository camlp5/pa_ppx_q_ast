
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
# 32 "sexp_parser.mly"
       (string)
# 19 "sexp_parser.ml"
  )
    | ANTI_ATOM of (
# 35 "sexp_parser.mly"
       (string * Location.t)
# 24 "sexp_parser.ml"
  )
    | ANTI of (
# 34 "sexp_parser.mly"
       (string * Location.t)
# 29 "sexp_parser.ml"
  )
  
end

include MenhirBasics

# 4 "sexp_parser.mly"
  

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

let vaval x = Ploc.VaVal x


# 59 "sexp_parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_parse_pattern_sexp) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: parse_pattern_sexp. *)

  | MenhirState01 : (('s, _menhir_box_parse_pattern_sexp) _menhir_cell1_LPAREN, _menhir_box_parse_pattern_sexp) _menhir_state
    (** State 01.
        Stack shape : LPAREN.
        Start symbol: parse_pattern_sexp. *)

  | MenhirState08 : (('s, _menhir_box_parse_pattern_sexp) _menhir_cell1_pattern_sexp, _menhir_box_parse_pattern_sexp) _menhir_state
    (** State 08.
        Stack shape : pattern_sexp.
        Start symbol: parse_pattern_sexp. *)

  | MenhirState09 : ((('s, _menhir_box_parse_pattern_sexp) _menhir_cell1_pattern_sexp, _menhir_box_parse_pattern_sexp) _menhir_cell1_DOT, _menhir_box_parse_pattern_sexp) _menhir_state
    (** State 09.
        Stack shape : pattern_sexp DOT.
        Start symbol: parse_pattern_sexp. *)

  | MenhirState15 : ('s, _menhir_box_parse_sexp) _menhir_state
    (** State 15.
        Stack shape : .
        Start symbol: parse_sexp. *)

  | MenhirState16 : (('s, _menhir_box_parse_sexp) _menhir_cell1_LPAREN, _menhir_box_parse_sexp) _menhir_state
    (** State 16.
        Stack shape : LPAREN.
        Start symbol: parse_sexp. *)

  | MenhirState21 : (('s, _menhir_box_parse_sexp) _menhir_cell1_sexp, _menhir_box_parse_sexp) _menhir_state
    (** State 21.
        Stack shape : sexp.
        Start symbol: parse_sexp. *)

  | MenhirState22 : ((('s, _menhir_box_parse_sexp) _menhir_cell1_sexp, _menhir_box_parse_sexp) _menhir_cell1_DOT, _menhir_box_parse_sexp) _menhir_state
    (** State 22.
        Stack shape : sexp DOT.
        Start symbol: parse_sexp. *)


and ('s, 'r) _menhir_cell1_pattern_sexp = 
  | MenhirCell1_pattern_sexp of 's * ('s, 'r) _menhir_state * (Sexp.Pattern.sexp) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_sexp = 
  | MenhirCell1_sexp of 's * ('s, 'r) _menhir_state * (Sexp.Normal.sexp) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_DOT = 
  | MenhirCell1_DOT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state * Lexing.position

and _menhir_box_parse_sexp = 
  | MenhirBox_parse_sexp of (Sexp.Normal.sexp) [@@unboxed]

and _menhir_box_parse_pattern_sexp = 
  | MenhirBox_parse_pattern_sexp of (Sexp.Pattern.sexp) [@@unboxed]

let _menhir_action_02 =
  fun _1 ->
    (
# 104 "sexp_parser.mly"
    ( _1 )
# 126 "sexp_parser.ml"
     : (Sexp.Pattern.sexp))

let _menhir_action_03 =
  fun _1 ->
    (
# 99 "sexp_parser.mly"
    ( _1 )
# 134 "sexp_parser.ml"
     : (Sexp.Normal.sexp))

let _menhir_action_04 =
  fun _endpos__2_ _startpos__1_ ->
    let _endpos = _endpos__2_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 127 "sexp_parser.mly"
      ( Pattern.Nil (make_loc _sloc) )
# 145 "sexp_parser.ml"
     : (Sexp.Pattern.sexp))

let _menhir_action_05 =
  fun l ->
    (
# 129 "sexp_parser.mly"
      ( l )
# 153 "sexp_parser.ml"
     : (Sexp.Pattern.sexp))

let _menhir_action_06 =
  fun _1 _endpos__1_ _startpos__1_ ->
    let a = 
# 90 "sexp_parser.mly"
     ( vaval _1 )
# 161 "sexp_parser.ml"
     in
    let (_endpos_a_, _startpos_a_) = (_endpos__1_, _startpos__1_) in
    let _endpos = _endpos_a_ in
    let _symbolstartpos = _startpos_a_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 131 "sexp_parser.mly"
      ( Pattern.Atom (make_loc _sloc, a) )
# 170 "sexp_parser.ml"
     : (Sexp.Pattern.sexp))

let _menhir_action_07 =
  fun _1 _endpos__1_ _startpos__1_ ->
    let a = 
# 92 "sexp_parser.mly"
     ( Ploc.VaAnt (fst _1) )
# 178 "sexp_parser.ml"
     in
    let (_endpos_a_, _startpos_a_) = (_endpos__1_, _startpos__1_) in
    let _endpos = _endpos_a_ in
    let _symbolstartpos = _startpos_a_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 131 "sexp_parser.mly"
      ( Pattern.Atom (make_loc _sloc, a) )
# 187 "sexp_parser.ml"
     : (Sexp.Pattern.sexp))

let _menhir_action_08 =
  fun a ->
    (
# 133 "sexp_parser.mly"
    ( Pattern.Xtra (snd a, fst a) )
# 195 "sexp_parser.ml"
     : (Sexp.Pattern.sexp))

let _menhir_action_09 =
  fun _endpos_l_ _startpos_l_ l ->
    let _endpos = _endpos_l_ in
    let _symbolstartpos = _startpos_l_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 138 "sexp_parser.mly"
    ( Pattern.Cons (make_loc _sloc, l, Pattern.Nil (make_loc _sloc)) )
# 206 "sexp_parser.ml"
     : (Sexp.Pattern.sexp))

let _menhir_action_10 =
  fun _endpos_r_ _startpos_l_ l r ->
    let _endpos = _endpos_r_ in
    let _symbolstartpos = _startpos_l_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 140 "sexp_parser.mly"
    ( Pattern.Cons (make_loc _sloc, l, r) )
# 217 "sexp_parser.ml"
     : (Sexp.Pattern.sexp))

let _menhir_action_11 =
  fun _endpos_r_ _startpos_l_ l r ->
    let _endpos = _endpos_r_ in
    let _symbolstartpos = _startpos_l_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 142 "sexp_parser.mly"
    ( Pattern.Cons (make_loc _sloc, l, r) )
# 228 "sexp_parser.ml"
     : (Sexp.Pattern.sexp))

let _menhir_action_12 =
  fun _endpos__2_ _startpos__1_ ->
    let _endpos = _endpos__2_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 110 "sexp_parser.mly"
      ( Normal.Nil (make_loc _sloc) )
# 239 "sexp_parser.ml"
     : (Sexp.Normal.sexp))

let _menhir_action_13 =
  fun l ->
    (
# 112 "sexp_parser.mly"
      ( l )
# 247 "sexp_parser.ml"
     : (Sexp.Normal.sexp))

let _menhir_action_14 =
  fun _endpos_a_ _startpos_a_ a ->
    let _endpos = _endpos_a_ in
    let _symbolstartpos = _startpos_a_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 114 "sexp_parser.mly"
      ( Normal.Atom (make_loc _sloc, a) )
# 258 "sexp_parser.ml"
     : (Sexp.Normal.sexp))

let _menhir_action_15 =
  fun _endpos_l_ _startpos_l_ l ->
    let _endpos = _endpos_l_ in
    let _symbolstartpos = _startpos_l_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 119 "sexp_parser.mly"
    ( Normal.Cons (make_loc _sloc, l, Normal.Nil (make_loc _sloc)) )
# 269 "sexp_parser.ml"
     : (Sexp.Normal.sexp))

let _menhir_action_16 =
  fun _endpos_r_ _startpos_l_ l r ->
    let _endpos = _endpos_r_ in
    let _symbolstartpos = _startpos_l_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 121 "sexp_parser.mly"
    ( Normal.Cons (make_loc _sloc, l, r) )
# 280 "sexp_parser.ml"
     : (Sexp.Normal.sexp))

let _menhir_action_17 =
  fun _endpos_r_ _startpos_l_ l r ->
    let _endpos = _endpos_r_ in
    let _symbolstartpos = _startpos_l_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 123 "sexp_parser.mly"
    ( Normal.Cons (make_loc _sloc, l, r) )
# 291 "sexp_parser.ml"
     : (Sexp.Normal.sexp))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ANTI _ ->
        "ANTI"
    | ANTI_ATOM _ ->
        "ANTI_ATOM"
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
  
  let rec _menhir_run_12 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_parse_pattern_sexp =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let _1 = _v in
          let _v = _menhir_action_02 _1 in
          MenhirBox_parse_pattern_sexp _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_parse_pattern_sexp) _menhir_state -> _menhir_box_parse_pattern_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_startpos__1_, _endpos__2_) = (_startpos, _endpos) in
          let _v = _menhir_action_04 _endpos__2_ _startpos__1_ in
          _menhir_goto_pattern_sexp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _startpos__1_ _v _menhir_s _tok
      | LPAREN ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | ATOM _v ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
          let _startpos_0 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos_0, _v) in
          let _v = _menhir_action_06 _1 _endpos__1_ _startpos__1_ in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v MenhirState01 _tok
      | ANTI_ATOM _v ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
          let _startpos_1 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos_1, _v) in
          let _v = _menhir_action_07 _1 _endpos__1_ _startpos__1_ in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v MenhirState01 _tok
      | ANTI _v ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
          let _startpos_2 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_endpos_a_, _startpos_a_, a) = (_endpos, _startpos_2, _v) in
          let _v = _menhir_action_08 a in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_a_ _startpos_a_ _v MenhirState01 _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_pattern_sexp : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_parse_pattern_sexp) _menhir_state -> _ -> _menhir_box_parse_pattern_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_12 _menhir_stack _v _tok
      | MenhirState09 ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState08 ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState01 ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
  
  and _menhir_run_10 : type  ttv_stack. ((ttv_stack, _menhir_box_parse_pattern_sexp) _menhir_cell1_pattern_sexp, _menhir_box_parse_pattern_sexp) _menhir_cell1_DOT -> _ -> _ -> _ -> _ -> _ -> _menhir_box_parse_pattern_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_DOT (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_pattern_sexp (_menhir_stack, _menhir_s, l, _startpos_l_, _) = _menhir_stack in
      let (_endpos_r_, r) = (_endpos, _v) in
      let _v = _menhir_action_11 _endpos_r_ _startpos_l_ l r in
      _menhir_goto_pattern_sexp_list _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_r_ _v _menhir_s _tok
  
  and _menhir_goto_pattern_sexp_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_parse_pattern_sexp) _menhir_state -> _ -> _menhir_box_parse_pattern_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState08 ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState01 ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_11 : type  ttv_stack. (ttv_stack, _menhir_box_parse_pattern_sexp) _menhir_cell1_pattern_sexp -> _ -> _ -> _ -> _ -> _ -> _menhir_box_parse_pattern_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_pattern_sexp (_menhir_stack, _menhir_s, l, _startpos_l_, _) = _menhir_stack in
      let (_endpos_r_, r) = (_endpos, _v) in
      let _v = _menhir_action_10 _endpos_r_ _startpos_l_ l r in
      _menhir_goto_pattern_sexp_list _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_r_ _v _menhir_s _tok
  
  and _menhir_run_06 : type  ttv_stack. (ttv_stack, _menhir_box_parse_pattern_sexp) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> _menhir_box_parse_pattern_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (l, _endpos__3_) = (_v, _endpos_0) in
          let _v = _menhir_action_05 l in
          _menhir_goto_pattern_sexp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_08 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_parse_pattern_sexp) _menhir_state -> _ -> _menhir_box_parse_pattern_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_stack = MenhirCell1_pattern_sexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState08
      | DOT ->
          let _menhir_stack = MenhirCell1_pattern_sexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_stack = MenhirCell1_DOT (_menhir_stack, MenhirState08) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState09
          | ATOM _v_0 ->
              let _startpos_1 = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos_2 = _menhir_lexbuf.Lexing.lex_curr_p in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let (_endpos__1_, _startpos__1_, _1) = (_endpos_2, _startpos_1, _v_0) in
              let _v = _menhir_action_06 _1 _endpos__1_ _startpos__1_ in
              _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _tok
          | ANTI_ATOM _v_4 ->
              let _startpos_5 = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos_6 = _menhir_lexbuf.Lexing.lex_curr_p in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let (_endpos__1_, _startpos__1_, _1) = (_endpos_6, _startpos_5, _v_4) in
              let _v = _menhir_action_07 _1 _endpos__1_ _startpos__1_ in
              _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _tok
          | ANTI _v_8 ->
              let _endpos_10 = _menhir_lexbuf.Lexing.lex_curr_p in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let (_endpos_a_, a) = (_endpos_10, _v_8) in
              let _v = _menhir_action_08 a in
              _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_a_ _v _tok
          | _ ->
              _eRR ())
      | ATOM _v_12 ->
          let _menhir_stack = MenhirCell1_pattern_sexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _startpos_13 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos_14 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_endpos__1_, _startpos__1_, _1) = (_endpos_14, _startpos_13, _v_12) in
          let _v = _menhir_action_06 _1 _endpos__1_ _startpos__1_ in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v MenhirState08 _tok
      | ANTI_ATOM _v_16 ->
          let _menhir_stack = MenhirCell1_pattern_sexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _startpos_17 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos_18 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_endpos__1_, _startpos__1_, _1) = (_endpos_18, _startpos_17, _v_16) in
          let _v = _menhir_action_07 _1 _endpos__1_ _startpos__1_ in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v MenhirState08 _tok
      | ANTI _v_20 ->
          let _menhir_stack = MenhirCell1_pattern_sexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _startpos_21 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos_22 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_endpos_a_, _startpos_a_, a) = (_endpos_22, _startpos_21, _v_20) in
          let _v = _menhir_action_08 a in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_a_ _startpos_a_ _v MenhirState08 _tok
      | RPAREN ->
          let (_endpos_l_, _startpos_l_, l) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_09 _endpos_l_ _startpos_l_ l in
          _menhir_goto_pattern_sexp_list _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_l_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  let rec _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_parse_pattern_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | ATOM _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_06 _1 _endpos__1_ _startpos__1_ in
          _menhir_run_12 _menhir_stack _v _tok
      | ANTI_ATOM _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_endpos__1_, _startpos__1_, _1) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_07 _1 _endpos__1_ _startpos__1_ in
          _menhir_run_12 _menhir_stack _v _tok
      | ANTI _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let a = _v in
          let _v = _menhir_action_08 a in
          _menhir_run_12 _menhir_stack _v _tok
      | _ ->
          _eRR ()
  
  let rec _menhir_run_25 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_parse_sexp =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let _1 = _v in
          let _v = _menhir_action_03 _1 in
          MenhirBox_parse_sexp _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_16 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_parse_sexp) _menhir_state -> _menhir_box_parse_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_startpos__1_, _endpos__2_) = (_startpos, _endpos) in
          let _v = _menhir_action_12 _endpos__2_ _startpos__1_ in
          _menhir_goto_sexp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _startpos__1_ _v _menhir_s _tok
      | LPAREN ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState16
      | ATOM _v ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
          let _startpos_0 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_endpos_a_, _startpos_a_, a) = (_endpos, _startpos_0, _v) in
          let _v = _menhir_action_14 _endpos_a_ _startpos_a_ a in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_a_ _startpos_a_ _v MenhirState16 _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_sexp : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_parse_sexp) _menhir_state -> _ -> _menhir_box_parse_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState15 ->
          _menhir_run_25 _menhir_stack _v _tok
      | MenhirState22 ->
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState21 ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState16 ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
  
  and _menhir_run_23 : type  ttv_stack. ((ttv_stack, _menhir_box_parse_sexp) _menhir_cell1_sexp, _menhir_box_parse_sexp) _menhir_cell1_DOT -> _ -> _ -> _ -> _ -> _ -> _menhir_box_parse_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_DOT (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_sexp (_menhir_stack, _menhir_s, l, _startpos_l_, _) = _menhir_stack in
      let (_endpos_r_, r) = (_endpos, _v) in
      let _v = _menhir_action_17 _endpos_r_ _startpos_l_ l r in
      _menhir_goto_sexp_list _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_r_ _v _menhir_s _tok
  
  and _menhir_goto_sexp_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_parse_sexp) _menhir_state -> _ -> _menhir_box_parse_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState21 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState16 ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_24 : type  ttv_stack. (ttv_stack, _menhir_box_parse_sexp) _menhir_cell1_sexp -> _ -> _ -> _ -> _ -> _ -> _menhir_box_parse_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_sexp (_menhir_stack, _menhir_s, l, _startpos_l_, _) = _menhir_stack in
      let (_endpos_r_, r) = (_endpos, _v) in
      let _v = _menhir_action_16 _endpos_r_ _startpos_l_ l r in
      _menhir_goto_sexp_list _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_r_ _v _menhir_s _tok
  
  and _menhir_run_19 : type  ttv_stack. (ttv_stack, _menhir_box_parse_sexp) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> _menhir_box_parse_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (l, _endpos__3_) = (_v, _endpos_0) in
          let _v = _menhir_action_13 l in
          _menhir_goto_sexp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_21 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_parse_sexp) _menhir_state -> _ -> _menhir_box_parse_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_stack = MenhirCell1_sexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState21
      | DOT ->
          let _menhir_stack = MenhirCell1_sexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_stack = MenhirCell1_DOT (_menhir_stack, MenhirState21) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState22
          | ATOM _v_0 ->
              let _startpos_1 = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos_2 = _menhir_lexbuf.Lexing.lex_curr_p in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let (_endpos_a_, _startpos_a_, a) = (_endpos_2, _startpos_1, _v_0) in
              let _v = _menhir_action_14 _endpos_a_ _startpos_a_ a in
              _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_a_ _v _tok
          | _ ->
              _eRR ())
      | ATOM _v_4 ->
          let _menhir_stack = MenhirCell1_sexp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _startpos_5 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos_6 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_endpos_a_, _startpos_a_, a) = (_endpos_6, _startpos_5, _v_4) in
          let _v = _menhir_action_14 _endpos_a_ _startpos_a_ a in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_a_ _startpos_a_ _v MenhirState21 _tok
      | RPAREN ->
          let (_endpos_l_, _startpos_l_, l) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_15 _endpos_l_ _startpos_l_ l in
          _menhir_goto_sexp_list _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_l_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  let rec _menhir_run_15 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_parse_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState15
      | ATOM _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_endpos_a_, _startpos_a_, a) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_14 _endpos_a_ _startpos_a_ a in
          _menhir_run_25 _menhir_stack _v _tok
      | _ ->
          _eRR ()
  
end

let parse_sexp =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_parse_sexp v = _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

let parse_pattern_sexp =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_parse_pattern_sexp v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

# 145 "sexp_parser.mly"
  

# 671 "sexp_parser.ml"
