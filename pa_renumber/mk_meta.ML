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

Fmt.(pf stdout {|
# Specifications for the "pa_ppx_q_ast_test_renumber" preprocessor:
requires = "camlp5,fmt,re,pcre2,pa_ppx.base"
version = "%s"
description = "pa_ppx_q_ast_test_renumber: pa_ppx_q_ast_test_renumber rewriter"

# For linking
package "link" (
requires = "camlp5,fmt,re,pcre2,pa_ppx.base.link"
archive(byte) = "pa_ppx_q_ast_test_renumber.cma"
archive(native) = "pa_ppx_q_ast_test_renumber.cmxa"
)

# For the toploop:
archive(byte,toploop) = "pa_ppx_q_ast_test_renumber.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt,re,pcre2,pa_ppx.base"
  archive(syntax,preprocessor,-native) = "pa_ppx_q_ast_test_renumber.cma"
  archive(syntax,preprocessor,native) = "pa_ppx_q_ast_test_renumber.cmxa"

|} version)
