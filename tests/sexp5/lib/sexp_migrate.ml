(** -syntax camlp5o -package pa_ppx_migrate,pa_ppx.import $(IMPORT_OCAMLCFLAGS) *)
exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)


module Regular = struct

[%%import: Sexp.sexp
  [@with Ploc.vala := vala]
  [@with Position.position := position]
  [@add [%%import: 'a Sexp.Ploc.vala]]
  [@add type position = [%import: Sexp.Position.position
                                    [@with Ploc.vala := vala]
                        ]
  ]
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
          srctype = [%typ: position]
        ; dsttype = [%typ: Sexp.Position.position]
        ; code = fun __dt__ x -> x
        }
      }
    }
]

let dt = make_dt ()
let sexp x = dt.migrate_sexp dt x

let reloc_sexp =
  let dt = make_dt () in
  let dt = {(dt) with migrate_loc = fun _ _ -> Sexp.Position.dummy_pos } in
  dt.migrate_sexp dt

end

module NoVala = struct

[%%import: Sexp.NoVala.sexp
  [@add [%%import: 'a Sexp.NoVala.novala]]
  [@with Position.position := position]
  [@add type position = [%import: Sexp.NoVala.Position.position]]
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
          srctype = [%typ: position]
        ; dsttype = [%typ: Sexp.NoVala.Position.position]
        ; code = fun __dt__ x -> x
        }
      }
    }
]

let dt = make_dt ()
let sexp x = dt.migrate_sexp dt x

let reloc_sexp =
  let dt = make_dt () in
  let dt = {(dt) with migrate_loc = fun _ _ -> Sexp.NoVala.Position.dummy_pos } in
  dt.migrate_sexp dt

end

module ToNoVala = struct

let _migrate_vala __subrw_0 __dt__ = function
    Ploc.VaAnt v_0 ->
    migration_error "Sexp_migrate.ToNoVala: found an antiquotation (not permitted when converting to NoVala)"
  | Ploc.VaVal v_0 -> (__subrw_0 __dt__ v_0)

[%%import: Sexp.sexp
  [@with Ploc.vala := vala]
  [@with Position.position := position]
  [@add [%%import: 'a Sexp.Ploc.vala]]
  [@add type position = [%import: Sexp.Position.position
                                    [@with Ploc.vala := vala]
                        ]
  ]
]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Sexp
        ; dstmod = Sexp.NoVala
        ; types = [
            sexp
          ; position
          ]
        }
      ]
    ; dispatchers = {
        migrate_vala = {
          srctype = [%typ: 'a vala]
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

module FromNoVala = struct

let _migrate_vala __subrw_0 __dt__ x =
  Ploc.VaVal (__subrw_0 __dt__ x)

[%%import: Sexp.NoVala.sexp
  [@add [%%import: 'a Sexp.NoVala.novala]]
  [@with Position.position := position]
  [@add type position = [%import: Sexp.NoVala.Position.position]]
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
          ; position
          ]
        }
      ]
    ; dispatchers = {
        migrate_vala = {
          srctype = [%typ: 'a novala]
        ; dsttype = [%typ: 'b Sexp.Ploc.vala]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; code = _migrate_vala
        }
      }
    }
]

let dt = make_dt ()
let sexp x = dt.migrate_sexp dt x

end
