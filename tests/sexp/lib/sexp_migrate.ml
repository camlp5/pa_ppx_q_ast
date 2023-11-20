(**pp -syntax camlp5o -package hashcons,pa_ppx_migrate,pa_ppx.import,pa_ppx_unique.runtime *)
exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)


module Regular = struct

[%%import: Sexp.sexp
  [@with Ploc.vala := vala]
  [@add [%%import: 'a Sexp.Ploc.vala]]
]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Sexp
        ; dstmod = Sexp
        ; types = [
            sexp
          ]
        }
      ; {
        srcmod = Ploc
      ; dstmod = Ploc
      ; types = [
          vala
        ]
      }
      ]
    ; dispatchers = {
       migrate_loc = {
          srctype = [%typ: Ploc.t]
        ; dsttype = [%typ: Ploc.t]
        ; code = fun __dt__ x -> x
        }
      }
    }
]

let dt = make_dt ()
let sexp x = dt.migrate_sexp dt x

let reloc_sexp =
  let dt = make_dt () in
  let dt = {(dt) with migrate_loc = fun _ _ -> Ploc.dummy } in
  dt.migrate_sexp dt

end

module NoVala = struct

[%%import: Sexp.NoVala.sexp
  [@add [%%import: 'a Sexp.NoVala.novala]]
]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Sexp.NoVala
        ; dstmod = Sexp.NoVala
        ; types = [
            sexp
          ; novala
          ]
        }
      ]
    ; dispatchers = {
       migrate_loc = {
          srctype = [%typ: Ploc.t]
        ; dsttype = [%typ: Ploc.t]
        ; code = fun __dt__ x -> x
        }
      }
    }
]

let dt = make_dt ()
let sexp x = dt.migrate_sexp dt x

let reloc_sexp =
  let dt = make_dt () in
  let dt = {(dt) with migrate_loc = fun _ _ -> Ploc.dummy } in
  dt.migrate_sexp dt

end

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
      ; migrate_loc = {
          srctype = [%typ: Ploc.t]
        ; dsttype = [%typ: Ploc.t]
        ; code = fun __dt__ x -> x
        }
      }
    }
]

let dt = make_dt ()
let sexp x = dt.migrate_sexp dt x

end

module FromNoVala = struct

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
      ; migrate_loc = {
          srctype = [%typ: Ploc.t]
        ; dsttype = [%typ: Ploc.t]
        ; code = fun __dt__ x -> x
        }
      }
    }
]

let dt = make_dt ()
let sexp x = dt.migrate_sexp dt x

end

module HC = struct

[%%import: Sexp_hashcons.HC.sexp
  [@with Ploc.vala := vala]
  [@add [%%import: 'a Sexp.Ploc.vala]]
]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Sexp_hashcons.HC
        ; dstmod = Sexp_hashcons.HC
        ; types = [
            sexp_node
          ]
        }
      ; {
        srcmod = Ploc
      ; dstmod = Ploc
      ; types = [
          vala
        ]
      }
      ]
    ; dispatchers = {
        migrate_sexp = {
          srctype = [%typ: sexp]
        ; dsttype = [%typ: Sexp_hashcons.HC.sexp]
        ; code = (fun __dt__ x ->
            Sexp_hashcons.HC.sexp (__dt__.migrate_sexp_node __dt__ x.Hashcons.node)
          )
        }
       ; migrate_loc = {
          srctype = [%typ: Ploc.t]
        ; dsttype = [%typ: Ploc.t]
        ; code = fun __dt__ x -> x
        }
      }
    }
]

let dt = make_dt ()
let sexp x = dt.migrate_sexp dt x

let reloc_sexp =
  let dt = make_dt () in
  let dt = {(dt) with migrate_loc = fun _ _ -> Ploc.dummy } in
  dt.migrate_sexp dt

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
      ; migrate_loc = {
          srctype = [%typ: Ploc.t]
        ; dsttype = [%typ: Ploc.t]
        ; code = fun __dt__ x -> x
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
      ; migrate_loc = {
          srctype = [%typ: Ploc.t]
        ; dsttype = [%typ: Ploc.t]
        ; code = fun __dt__ x -> x
        }
      }
    }
]

let dt = make_dt ()
let sexp x = dt.migrate_sexp dt x

end

module Unique = struct

[%%import: Sexp_unique.UN.sexp
  [@with Ploc.vala := vala]
  [@add [%%import: 'a Sexp.Ploc.vala]]
]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Sexp_unique.UN
        ; dstmod = Sexp_unique.UN
        ; types = [
            sexp_node
          ]
        }
      ; {
        srcmod = Ploc
      ; dstmod = Ploc
      ; types = [
          vala
        ]
      }
      ]
    ; dispatchers = {
        migrate_sexp = {
          srctype = [%typ: sexp]
        ; dsttype = [%typ: Sexp_unique.UN.sexp]
        ; code = (fun __dt__ x ->
            Sexp_unique.UN.sexp (__dt__.migrate_sexp_node __dt__ x.Pa_ppx_unique_runtime.Unique.node)
          )
        }
       ; migrate_loc = {
          srctype = [%typ: Ploc.t]
        ; dsttype = [%typ: Ploc.t]
        ; code = fun __dt__ x -> x
        }
      }
    }
]

let dt = make_dt ()
let sexp x = dt.migrate_sexp dt x

let reloc_sexp =
  let dt = make_dt () in
  let dt = {(dt) with migrate_loc = fun _ _ -> Ploc.dummy } in
  dt.migrate_sexp dt

end

module ToUnique = struct

[%%import: Sexp_unique.OK.sexp]
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
        ; dsttype = [%typ: Sexp_unique.UN.sexp_node]
        }
      ; migrate_sexp = {
          srctype = [%typ: sexp]
        ; dsttype = [%typ: Sexp_unique.UN.sexp]
        ; code = (fun __dt__ x ->
            Sexp_unique.UN.sexp (__dt__.migrate_sexp_node __dt__ x)
          )
        }
      ; migrate_loc = {
          srctype = [%typ: Ploc.t]
        ; dsttype = [%typ: Ploc.t]
        ; code = fun __dt__ x -> x
        }
      }
    }
]

let dt = make_dt ()
let sexp x = dt.migrate_sexp dt x

end

module FromUnique = struct
exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)

[%%import: Sexp_unique.UN.sexp]
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
        ; dsttype = [%typ: Sexp_unique.OK.sexp_node]
        }
      ; migrate_sexp = {
          srctype = [%typ: sexp]
        ; dsttype = [%typ: Sexp_unique.OK.sexp]
        ; code = (fun __dt__ x ->
            __dt__.migrate_sexp_node __dt__ x.Pa_ppx_unique_runtime.Unique.node
          )
        }
      ; migrate_loc = {
          srctype = [%typ: Ploc.t]
        ; dsttype = [%typ: Ploc.t]
        ; code = fun __dt__ x -> x
        }
      }
    }
]

let dt = make_dt ()
let sexp x = dt.migrate_sexp dt x

end


