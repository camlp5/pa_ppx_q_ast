(**pp -syntax camlp5o *)
(* camlp5o *)
(* sexp.ml,v *)

type t =
#if OCAML_VERSION >= (5,4,0)
    a: char * int32
#else
    char * int32
#endif
  
