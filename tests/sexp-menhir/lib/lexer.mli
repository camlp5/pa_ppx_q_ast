
val token: Lexing.lexbuf -> Parser.token

type error =
  | Illegal_character of char

exception Error of error * Location.t

