(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx_q_ast_quotation_test.full *)
type t = Types.t = A | B[@@deriving quotation_test { test_types = [t] }]
