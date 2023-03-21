#include "version.inc"

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

Fmt.(pf stdout {|# Specifications for the "pa_ppx_q_ast_mkast" preprocessor:
version = "%s"
description = "pa_ppx_q_ast_mkast rewriter"

  requires(toploop) = "camlp5,pa_ppx.deriving_plugins.show,camlp5.parser_quotations,pa_ppx.params_runtime"
  archive(toploop) = "pa_mkast.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx.deriving_plugins.show,camlp5.parser_quotations,pa_ppx.params_runtime"
    archive(syntax,preprocessor,-native) = "pa_mkast.cmo"
    archive(syntax,preprocessor,native) = "pa_mkast.cmx"

  package "link" (
  requires = "camlp5,pa_ppx.deriving_plugins.show.link,camlp5.parser_quotations.link"
  archive(byte) = "pa_mkast.cmo"
  archive(native) = "pa_mkast.cmx"
  )
|} version)
