(* tytostring.sig *)

signature TYTOSTRING =
sig
    val varToString  : Ens_types2.var_elem  list -> string
    val typeToString : Ens_types2.type_elem list -> string
    val consToString : Ens_types2.cons_elem list -> string
    val strToString  : Ens_types2.str_elem  list -> string
    val sigToString  : Ens_types2.sig_elem  list -> string
    val extToString  : Ens_types2.ext_elem  list -> string

    val lvarExtToString : (Access.access * Access.access) list -> string
    val pidOptionToString : PersStamps.persstamp option -> string

end
