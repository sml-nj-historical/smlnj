(* SERIALIZE_DB.sig *)

signature SERIALIZE_DB =
sig
    val varToString  : DBTypes.var_elem  list -> string
    val typeToString : DBTypes.type_elem list -> string
    val consToString : DBTypes.cons_elem list -> string
    val strToString  : DBTypes.str_elem  list -> string
    val sigToString  : DBTypes.sig_elem  list -> string
    val extToString  : DBTypes.ext_elem  list -> string

    val lvarExtToString : (Access.access * Access.access) list -> string
    val pidOptionToString : PersStamps.persstamp option -> string

end
