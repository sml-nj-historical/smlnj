(* varcon.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure VarCon : VARCON =
struct

local structure A  = Access
      structure T  = Types
      structure S  = Symbol
      structure SP = SymPath


in

datatype var
  = VALvar of 		              (* ordinary variables *)
      {path : SP.path,
       typ : T.ty ref,
       access : A.access,
       info : PrimOpId.primId}
  | OVLDvar of       	      	      (* overloaded identifier *)
      {name : S.symbol,
       options: {indicator: T.ty, variant: var} list ref,
       scheme: T.tyfun}
  | ERRORvar

type datacon = T.datacon                     

datatype value
  = VAL of var
  | CON of datacon

fun mkVALvar (id, acc) =
      VALvar{path = SP.SPATH [id], 
             typ = ref T.UNDEFty,
             access = acc,
             info = II.Null}

val bogusCON = T.DATACON{name=S.varSymbol "bogus",
                         typ=T.WILDCARDty,
                         rep=A.CONSTANT 0,
                         const=true,
			 lazyp=false,
                         sign=A.CSIG(0,1)}

val bogusEXN = T.DATACON{name=S.varSymbol "bogus",
			 typ=CoreBasicTypes.exnTy,
			 rep=A.CONSTANT 0,
			 const=true,
			 lazyp=false,
			 sign=A.CNIL}

end (* local *)
end (* structure VarCon *)
