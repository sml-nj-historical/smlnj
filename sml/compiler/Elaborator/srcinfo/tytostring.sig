(* tytostring.sig *)

signature TYTOSTRING =
sig

    val tyToString : Ens_types2.ty' -> string
    val tyconToString : Ens_types2.tycon' -> string
    val allToString : 
	Ens_types2.var_elem list * 
	Ens_types2.type_elem list * 
	Ens_types2.cons_elem list * 
	Ens_types2.str_elem list * 
	Ens_types2.sig_elem list -> string
end
