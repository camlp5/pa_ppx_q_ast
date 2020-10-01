
let _migrate_vala __subrw_0 __dt__ = function
    Ploc.VaAnt v_0 -> Ploc.VaAnt v_0
  | Ploc.VaVal v_0 -> Ploc.VaVal (__subrw_0 __dt__ v_0)

module Inject = struct
exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)

type lam_node = [%import: Lam_hashcons.lam]
and lam = lam_node
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
      ; migrate_lam_node = {
          srctype = [%typ: lam_node]
        ; dsttype = [%typ: Lam_hashcons.HC.lam_node]
        }
      ; migrate_lam = {
          srctype = [%typ: lam]
        ; dsttype = [%typ: Lam_hashcons.HC.lam]
        ; code = (fun __dt__ x ->
            Lam_hashcons.HC.lam (__dt__.migrate_lam_node __dt__ x)
          )
        }
      }
    }
]

let dt = make_dt ()
let inject x = dt.migrate_lam dt x

end

module Project = struct
exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)

[%%import: Lam_hashcons.HC.lam]
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
      ; migrate_lam_node = {
          srctype = [%typ: lam_node]
        ; dsttype = [%typ: Lam_hashcons.lam]
        }
      ; migrate_lam = {
          srctype = [%typ: lam]
        ; dsttype = [%typ: Lam_hashcons.lam]
        ; code = (fun __dt__ x ->
            __dt__.migrate_lam_node __dt__ x.Hashcons.node
          )
        }
      }
    }
]

let dt = make_dt ()
let inject x = dt.migrate_lam dt x

end

