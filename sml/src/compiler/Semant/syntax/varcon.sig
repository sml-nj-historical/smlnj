(* Copyright 1996 by AT&T Bell Laboratories *)
(* varcon.sig *)

signature VARCON = 
sig

  datatype var
    = VALvar of 		      (* ordinary variables *)
        {path : SymPath.path,
	 typ : Types.ty ref,
         access : Access.access,
         info   : InlInfo.inl_info}
    | OVLDvar of       	      	      (* overloaded identifier *)
	  {name : Symbol.symbol,
	   options: {indicator: Types.ty, variant: var} list ref,
	   scheme: Types.tyfun}
    | ERRORvar

  type datacon = Types.datacon

  datatype value
    = VAL of var
    | CON of datacon

  val mkVALvar : Symbol.symbol * Access.access ->  var

  val bogusCON : datacon
  val bogusEXN : datacon

end (* signature VARCON *)

(*
 * $Log$
 *)
