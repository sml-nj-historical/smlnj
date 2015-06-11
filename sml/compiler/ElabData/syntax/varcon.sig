(* varcon.sig
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
signature VARCON = 
sig

  datatype var
    = VALvar of				(* ordinary variables *)
        {path : SymPath.path,
	 typ : Types.ty ref,
	 btvs : Types.tyvar list ref,
         access : Access.access,
         prim   : PrimOpId.primId}
    | OVLDvar of			(* overloaded identifier *)
        {name : Symbol.symbol,
	 options: {indicator: Types.ty, variant: var} list,
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
