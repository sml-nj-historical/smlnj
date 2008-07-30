signature UNSERIALIZE_DB = 
sig
    val stringToVar : string -> DBTypes.var_elem list
    val stringToType : string -> DBTypes.type_elem list
    val stringToCons : string -> DBTypes.cons_elem list 
    val stringToStr : string -> DBTypes.str_elem list 
    val stringToSig : string -> DBTypes.sig_elem list
    val stringToExt : string -> DBTypes.ext_elem list

    val stringToOccurrenceList : string -> DBTypes.occurrence list
    val stringToLvarExt : string -> (Access.access * Access.access) list
    val stringToPidOption : string -> PersStamps.persstamp option
end (* signature UNSERIALIZE_DB *)
