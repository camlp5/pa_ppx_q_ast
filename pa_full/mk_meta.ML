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

Fmt.(pf stdout {|# Specifications for the "pa_ppx_q_ast_quotation_test_full" preprocessor:
version = "%s"
description = "pa_ppx_q_ast_quotation_test_full wrapper package"

requires(syntax,preprocessor) = "pa_ppx_q_ast_quotation_test,pa_ppx_q_ast_test_cleanup,pa_ppx_q_ast_test_renumber"
archive(syntax,preprocessor,-native) = "dummy.cmo"
archive(syntax,preprocessor,native) = "dummy.cmx"

requires = "pa_ppx_q_ast_quotation_test,pa_ppx_q_ast_test_cleanup,pa_ppx_q_ast_test_renumber"
archive(byte) = "dummy.cmo"
archive(native) = "dummy.cmx"

|} version)
