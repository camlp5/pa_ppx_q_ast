
let sexp s = s |> Lexing.from_string |> Sexp_parser.parse_sexp Sexp_lexer.token
