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

Fmt.(pf stdout {|# Specifications for "basic_example"
description = "sexp example parsing & quotation support"

package "runtime" (
  archive(toploop) = "basic.cmo"
  archive(syntax,preprocessor,-native) = "basic.cmo"
  requires = "hashcons,pa_ppx_unique.runtime"
)

package "parser" (
  requires(toploop) = "camlp5,basic_example.runtime"
  archive(toploop) = "pa_basic.cmo"

    requires(syntax,preprocessor) = "camlp5,fmt,basic_example.runtime"
    archive(syntax,preprocessor,-native) = "pa_basic.cmo"
    archive(syntax,preprocessor,native) = "pa_basic.cmx"

  package "link" (
  requires(byte) = "camlp5,fmt,basic_example.runtime"
  archive(byte) = "pa_basic.cmo"
  )
  requires = "camlp5,fmt,basic_example.runtime"
)

package "parser_quotations" (
  requires(toploop) = "camlp5,basic_example.parser,pa_ppx.base,pa_ppx_q_ast.runtime"
  archive(toploop) = "q_ast_basic.cmo"

    requires(syntax,preprocessor) = "camlp5,fmt,basic_example.runtime,basic_example.parser,camlp5.parser_quotations,pa_ppx_q_ast.runtime,pa_ppx.base"
    archive(syntax,preprocessor,-native) = "q_ast_basic.cmo"
    archive(syntax,preprocessor,native) = "q_ast_basic.cmx"

  package "link" (
  requires(byte) = "camlp5,fmt,basic_example.runtime,basic_example.parser,camlp5.parser_quotations,pa_ppx.base.link"
  archive(byte) = "q_ast_basic.cmo"
  )
  requires = "camlp5,fmt,basic_example.runtime,basic_example.parser,camlp5.parser_quotations,pa_ppx.base"
)
|})
