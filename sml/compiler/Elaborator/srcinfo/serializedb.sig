(* SERIALIZE_DB.sig *)

(* module that serializes types defined in DBTypes in a type safe way *)
(* it works the following way :
 * string : s -> s 
   ASSUME strings do not contains whitespaces : ok with symbols but it WON'T 
   work if a path has a whitespace in it
 * int : i -> Int.toString i
 * tuples : (a,b) -> (serialize a ^ " " ^ serialize b)
 * lists : [a,b] -> ("2" ^ " " ^ serialise a ^ " " serialize b)
 * datatype : A | B of b -> ("A") or ("B" ^ " " ^ serialize b)
 * all basics elements do not contain any whitespace so that no confusion is
 * possible
 *)
signature SERIALIZE_DB =
sig
    val varToString  : DBTypes.var_elem  list -> string
    val typeToString : DBTypes.type_elem list -> string
    val consToString : DBTypes.cons_elem list -> string
    val strToString  : DBTypes.str_elem  list -> string
    val sigToString  : DBTypes.sig_elem  list -> string
    val extToString  : DBTypes.ext_elem  list -> string

    val occurrenceListToString : DBTypes.occurrence list -> string
    val lvarExtToString : (Access.access * Access.access) list -> string
    val pidOptionToString : PersStamps.persstamp option -> string

end
