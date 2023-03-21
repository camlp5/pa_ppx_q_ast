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

Fmt.(pf stdout {|# Specifications for the "pa_ppx_q_ast_mktest" preprocessor:
version = "%s"
description = "pa_ppx_q_ast_mktest rewriter"

  requires(toploop) = "pa_ppx.base"
  archive(toploop) = "pa_ppx_mktest.cmo"

    requires(syntax,preprocessor) = "pa_ppx.base"
    archive(syntax,preprocessor,-native) = "pa_ppx_mktest.cmo"
    archive(syntax,preprocessor,native) = "pa_ppx_mktest.cmx"

  package "link" (
  requires = "pa_ppx.base"
  archive(byte) = "pa_ppx_mktest.cmo"
  archive(native) = "pa_ppx_mktest.cmx"
  )
|} version)
