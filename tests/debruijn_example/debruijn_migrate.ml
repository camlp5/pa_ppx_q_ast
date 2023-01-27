(** $(IMPORT_OCAMLCFLAGS) -package pa_ppx_q_ast,pa_ppx.import,pa_ppx_hashcons,pa_ppx_migrate -syntax camlp5o *)
let _migrate_vala __subrw_0 __dt__ = function
    Ploc.VaAnt v_0 -> Ploc.VaAnt v_0
  | Ploc.VaVal v_0 -> Ploc.VaVal (__subrw_0 __dt__ v_0)

module ToHC = struct
exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)

[%%import: Debruijn_hashcons.OK.term]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; dispatchers = {
        migrate_vala = {
          srctype = [%typ: 'a Ploc.vala]
        ; dsttype = [%typ: 'b Ploc.vala]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; code = _migrate_vala
        }
      ; migrate_term_node = {
          srctype = [%typ: term_node]
        ; dsttype = [%typ: Debruijn_hashcons.HC.term_node]
        }
      ; migrate_term = {
          srctype = [%typ: term]
        ; dsttype = [%typ: Debruijn_hashcons.HC.term]
        ; code = (fun __dt__ x ->
            Debruijn_hashcons.HC.term (__dt__.migrate_term_node __dt__ x)
          )
        }
      }
    }
]

let dt = make_dt ()
let term x = dt.migrate_term dt x

end

module FromHC = struct
exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)

[%%import: Debruijn_hashcons.HC.term]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; dispatchers = {
        migrate_vala = {
          srctype = [%typ: 'a Ploc.vala]
        ; dsttype = [%typ: 'b Ploc.vala]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; code = _migrate_vala
        }
      ; migrate_term_node = {
          srctype = [%typ: term_node]
        ; dsttype = [%typ: Debruijn_hashcons.OK.term_node]
        }
      ; migrate_term = {
          srctype = [%typ: term]
        ; dsttype = [%typ: Debruijn_hashcons.OK.term]
        ; code = (fun __dt__ x ->
            __dt__.migrate_term_node __dt__ x.Hashcons.node
          )
        }
      }
    }
]

let dt = make_dt ()
let term x = dt.migrate_term dt x

end

