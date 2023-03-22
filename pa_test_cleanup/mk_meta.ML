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

Fmt.(pf stdout {|# Specifications for the "pa_ppx_q_ast_test_cleanup" preprocessor:
version = "%s"
description = "pa_ppx_q_ast_test_cleanup rewriter"

  requires(toploop) = "pa_ppx.base,pa_ppx.deriving"
  archive(toploop) = "pa_ppx_test_cleanup.cmo"

    requires(syntax,preprocessor) = "pa_ppx.base,pa_ppx.deriving"
    archive(syntax,preprocessor,-native) = "pa_ppx_test_cleanup.cmo"
    archive(syntax,preprocessor,native) = "pa_ppx_test_cleanup.cmx"

  package "link" (
  requires = "pa_ppx.base,pa_ppx.deriving.link"
  archive(byte) = "pa_ppx_test_cleanup.cmo"
  archive(native) = "pa_ppx_test_cleanup.cmx"
  )
|} version)
