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

Fmt.(pf stdout {|# Specifications for the "pa_ppx_q_ast" preprocessor:
version = "%s"
description = "pa_ppx_q_ast deriver"

  package "runtime" (
    requires = "camlp5.parser_quotations"
    archive(byte) = "pa_ppx_q_ast_runtime.cmo"
    archive(native) = "pa_ppx_q_ast_runtime.cmx"
  )

  requires(toploop) = "camlp5,pa_ppx.deriving_plugins.show,camlp5.parser_quotations,pa_ppx.params_runtime,pa_ppx_q_ast.runtime"
  archive(toploop) = "pa_deriving_q_ast.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx.deriving_plugins.show,camlp5.parser_quotations,pa_ppx.params_runtime,pa_ppx_q_ast.runtime"
    archive(syntax,preprocessor,-native) = "pa_deriving_q_ast.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_q_ast.cmx"

  package "link" (
  requires = "camlp5,pa_ppx.deriving_plugins.show.link,camlp5.parser_quotations.link,pa_ppx_q_ast.runtime"
  archive(byte) = "pa_deriving_q_ast.cmo"
  archive(native) = "pa_deriving_q_ast.cmx"
  )
|} version)
