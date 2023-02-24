let top = try Sys.getenv "TOP" with Not_found -> "" ;;
let destdir = ref (top ^ "/local-install") ;;

Arg.(parse [
         "-destdir", (Arg.String (fun s -> destdir := s)),
         ("<path>    Set the destination directory (default: " ^
	    !destdir ^ ")")
       ]
       (fun _ -> failwith "mk_meta: no anonymous args supported")
     "mk_meta -destdir <dir>")
;;

Fmt.(pf stdout {|# Specifications for "sexp2_example"
description = "sexp example parsing & quotation support"

package "runtime" (
  archive(toploop) = "sexp.cmo"
  archive(syntax,preprocessor,-native) = "sexp.cmo"
  requires = "hashcons,pa_ppx_unique.runtime"
)

package "parser" (
  requires(toploop) = "camlp5,sexp2_example.runtime"
  archive(toploop) = "pa_sexp.cmo"

    requires(syntax,preprocessor) = "camlp5,fmt,sexp2_example.runtime"
    archive(syntax,preprocessor,-native) = "pa_sexp.cmo"
    archive(syntax,preprocessor,native) = "pa_sexp.cmx"

  package "link" (
  requires(byte) = "camlp5,fmt,sexp2_example.runtime"
  archive(byte) = "pa_sexp.cmo"
  )
  requires = "camlp5,fmt,sexp2_example.runtime"
)

package "parser_quotations" (
  requires(toploop) = "camlp5,sexp2_example.parser,pa_ppx.base,pa_ppx_q_ast.runtime"
  archive(toploop) = "q_ast_sexp.cmo"

    requires(syntax,preprocessor) = "camlp5,fmt,sexp2_example.runtime,sexp2_example.parser,camlp5.parser_quotations,pa_ppx_q_ast.runtime,pa_ppx.base"
    archive(syntax,preprocessor,-native) = "q_ast_sexp.cmo"
    archive(syntax,preprocessor,native) = "q_ast_sexp.cmx"

  package "link" (
  requires(byte) = "camlp5,fmt,sexp2_example.runtime,sexp2_example.parser,camlp5.parser_quotations,pa_ppx.base.link"
  archive(byte) = "q_ast_sexp.cmo"
  )
  requires = "camlp5,fmt,sexp2_example.runtime,sexp2_example.parser,camlp5.parser_quotations,pa_ppx.base"
)
|})
