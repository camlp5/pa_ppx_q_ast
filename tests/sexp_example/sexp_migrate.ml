
exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)

module ToNoVala = struct

let _migrate_vala __subrw_0 __dt__ = function
    Ploc.VaAnt v_0 ->
    migration_error "Sexp_migrate.ToNoVala: found an antiquotation (not permitted when converting to NoVala)"
  | Ploc.VaVal v_0 -> (__subrw_0 __dt__ v_0)

[%%import: Sexp.sexp]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Sexp
        ; dstmod = Sexp.NoVala
        ; types = [
            sexp
          ]
        }
      ]
    ; dispatchers = {
        migrate_vala = {
          srctype = [%typ: 'a Ploc.vala]
        ; dsttype = [%typ: 'b Sexp.NoVala.novala]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; code = _migrate_vala
        }
      }
    }
]

let dt = make_dt ()
let sexp x = dt.migrate_sexp dt x

end

module FromMoVala = struct

let _migrate_vala __subrw_0 __dt__ x =
  Ploc.VaVal (__subrw_0 __dt__ x)

[%%import: Sexp.NoVala.sexp
  [@with novala := Sexp.NoVala.novala]
]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Sexp.NoVala
        ; dstmod = Sexp
        ; types = [
            sexp
          ]
        }
      ]
    ; dispatchers = {
        migrate_vala = {
          srctype = [%typ: 'a Sexp.NoVala.novala]
        ; dsttype = [%typ: 'b Ploc.vala]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; code = _migrate_vala
        }
      }
    }
]

let dt = make_dt ()
let sexp x = dt.migrate_sexp dt x

end

let _migrate_vala __subrw_0 __dt__ = function
    Ploc.VaAnt v_0 -> Ploc.VaAnt v_0
  | Ploc.VaVal v_0 -> Ploc.VaVal (__subrw_0 __dt__ v_0)

module ToHC = struct

[%%import: Sexp_hashcons.OK.sexp]
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
      ; migrate_sexp_node = {
          srctype = [%typ: sexp_node]
        ; dsttype = [%typ: Sexp_hashcons.HC.sexp_node]
        }
      ; migrate_sexp = {
          srctype = [%typ: sexp]
        ; dsttype = [%typ: Sexp_hashcons.HC.sexp]
        ; code = (fun __dt__ x ->
            Sexp_hashcons.HC.sexp (__dt__.migrate_sexp_node __dt__ x)
          )
        }
      }
    }
]

let dt = make_dt ()
let sexp x = dt.migrate_sexp dt x

end

module FromHC = struct
exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)

[%%import: Sexp_hashcons.HC.sexp]
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
      ; migrate_sexp_node = {
          srctype = [%typ: sexp_node]
        ; dsttype = [%typ: Sexp_hashcons.OK.sexp_node]
        }
      ; migrate_sexp = {
          srctype = [%typ: sexp]
        ; dsttype = [%typ: Sexp_hashcons.OK.sexp]
        ; code = (fun __dt__ x ->
            __dt__.migrate_sexp_node __dt__ x.Hashcons.node
          )
        }
      }
    }
]

let dt = make_dt ()
let sexp x = dt.migrate_sexp dt x

end
