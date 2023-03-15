
val token: Lexing.lexbuf -> Sexp_parser.token

type error =
  | Illegal_character of char

exception Error of error * Location.t

