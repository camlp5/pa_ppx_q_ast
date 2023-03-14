
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

type ('s, 'r) _menhir_state

and _menhir_box_parse_sexp = 
  | MenhirBox_parse_sexp of (Sexp.sexp) [@@unboxed]

let _menhir_action_1 =
  fun _1 ->
    (
# 80 "parser.mly"
    ( _1 )
# 59 "parser.ml"
     : (Sexp.sexp))

let _menhir_action_2 =
  fun _endpos__2_ _startpos__1_ ->
    let _endpos = _endpos__2_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    (
# 86 "parser.mly"
        ( Nil (make_loc _sloc) )
# 70 "parser.ml"
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
  
  let rec _menhir_run_0 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_parse_sexp =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | RPAREN ->
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let (_endpos__2_, _startpos__1_) = (_endpos, _startpos) in
              let _v = _menhir_action_2 _endpos__2_ _startpos__1_ in
              (match (_tok : MenhirBasics.token) with
              | EOF ->
                  let _1 = _v in
                  let _v = _menhir_action_1 _1 in
                  MenhirBox_parse_sexp _v
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
end

let parse_sexp =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_parse_sexp v = _menhir_run_0 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

# 89 "parser.mly"
  

# 132 "parser.ml"
