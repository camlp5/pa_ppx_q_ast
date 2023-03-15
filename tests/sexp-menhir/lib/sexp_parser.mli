
(* The type of tokens. *)

type token = 
  | RPAREN
  | LPAREN
  | EOF
  | DOT
  | ATOM of (string)
  | ANTI_ATOM of (string)
  | ANTI of (string)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val parse_sexp: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Sexp.Normal.sexp)

val parse_pattern_sexp: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Sexp.Pattern.sexp)
