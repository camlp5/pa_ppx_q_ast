(**pp -syntax camlp5o -package pa_ppx.import *)
[%%import: Types.t13
]
[@@deriving quotation_test {
        test_types = [t13]
      ; minimal_record_module_labels = true
      ; expand_types = [
          ([%typ: t13b], Auto)
        ; ([%typ: t13b Ploc.vala option Ploc.vala], 
           AddDel(
               [],
               [ox
               ; Some x
               ]
             )
          )
        ]
  }]
