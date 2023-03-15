
let sexp s = s |> Lexing.from_string |> Sexp_parser.parse_sexp Sexp_lexer.token
let pattern_sexp s = s |> Lexing.from_string |> Sexp_parser.parse_pattern_sexp Sexp_lexer.token
