(* Copyright 1996 by AT&T Bell Laboratories *)
(* varcon.sml *)

structure VarCon : VARCON = 
struct

local structure A  = Access
      structure BT = BasicTypes
      structure II = InlInfo
      structure T  = Types
      structure S  = Symbol
      structure SP = SymPath


in

datatype var
  = VALvar of 		              (* ordinary variables *)
      {path : SP.path,
       typ : T.ty ref,
       access : A.access,
       info : II.inl_info}
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
             info = II.nullInfo}

val bogusCON = T.DATACON{name=S.varSymbol "bogus",
                         typ=T.WILDCARDty,
                         rep=A.CONSTANT 0,
                         const=true,
			 lazyp=false,
                         sign=A.CSIG(0,1)}

val bogusEXN = T.DATACON{name=S.varSymbol "bogus",
                         typ=BT.exnTy,
                         rep=A.CONSTANT 0,
                         const=true,
			 lazyp=false,
                         sign=A.CNIL}

end (* local *)
end (* structure VarCon *)

(*
 * $Log: varcon.sml,v $
 * Revision 1.3  1998/05/23 14:10:14  george
 *   Fixed RCS keyword syntax
 *
 *)
