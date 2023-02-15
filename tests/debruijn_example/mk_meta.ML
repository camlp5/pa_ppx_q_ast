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

Fmt.(pf stdout {|# Specifications for "debruijn_example"
description = "debruijn example parsing & quotation support"

package "runtime" (
  requires = "hashcons"
  archive(byte) = "debruijn.cmo debruijn_hashcons.cmo debruijn_migrate.cmo"
  archive(native) = "debruijn.cmx debruijn_hashcons.cmx debruijn_migrate.cmx"
)

package "parser" (
  requires(toploop) = "camlp5,debruijn_example.runtime"
  archive(toploop) = "pa_debruijn.cmo"

    requires(syntax,preprocessor) = "camlp5,fmt,debruijn_example.runtime,pa_ppx_q_ast.runtime"
    archive(syntax,preprocessor,-native) = "pa_debruijn.cmo"
    archive(syntax,preprocessor,native) = "pa_debruijn.cmx"

  package "link" (
  requires(byte) = "camlp5,fmt,debruijn_example.runtime"
  archive(byte) = "pa_debruijn.cmo"
  )
  requires = "camlp5,fmt,debruijn_example.runtime"
)

package "parser_quotations" (
  requires(toploop) = "camlp5,debruijn_example.parser"
  archive(toploop) = "q_ast_debruijn.cmo"

    requires(syntax,preprocessor) = "camlp5,fmt,debruijn_example.runtime,debruijn_example.parser,camlp5.parser_quotations"
    archive(syntax,preprocessor,-native) = "q_ast_debruijn.cmo"
    archive(syntax,preprocessor,native) = "q_ast_debruijn.cmx"

  package "link" (
  requires(byte) = "camlp5,fmt,debruijn_example.runtime,debruijn_example.parser,camlp5.parser_quotations"
  archive(byte) = "q_ast_debruijn.cmo"
  )
  requires = "camlp5,fmt,debruijn_example.runtime,debruijn_example.parser,camlp5.parser_quotations"
)
|})
