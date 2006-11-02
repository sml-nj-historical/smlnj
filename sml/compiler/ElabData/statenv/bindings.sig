(* bindings.sig
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
signature BINDINGS = sig

    datatype binding =
	VALbind of VarCon.var
      | CONbind of VarCon.datacon
      | TYCbind of Types.tycon
      | SIGbind of Modules.Signature
      | STRbind of Modules.Structure
      | FSGbind of Modules.fctSig
      | FCTbind of Modules.Functor
      | FIXbind of Fixity.fixity

    val binderGt :
	(Symbol.symbol * binding) * (Symbol.symbol * binding) -> bool

end (* signature BINDINGS *)
