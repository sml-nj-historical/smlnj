(* Copyright 1996 by AT&T Bell Laboratories *)
(* bindings.sml *)

signature BINDINGS =
sig

datatype binding
  = VALbind of VarCon.var
  | CONbind of VarCon.datacon
  | TYCbind of Types.tycon
  | SIGbind of Modules.Signature
  | STRbind of Modules.Structure
  | FSGbind of Modules.fctSig
  | FCTbind of Modules.Functor
  | FIXbind of Fixity.fixity

val binderGt : (Symbol.symbol * binding) * (Symbol.symbol * binding) -> bool

end (* signature BINDINGS *)


structure Bindings : BINDINGS =
struct

local structure S  = Symbol
      structure T  = Types
      structure V  = VarCon
      structure M =  Modules
in

fun err s = ErrorMsg.impossible ("Bindings: "^s)

datatype binding
  = VALbind of V.var
  | CONbind of V.datacon
  | TYCbind of T.tycon
  | SIGbind of M.Signature
  | STRbind of M.Structure
  | FSGbind of M.fctSig
  | FCTbind of M.Functor
  | FIXbind of Fixity.fixity

(* used for statenv sorting in env/statenv.sml *)
fun binderGt(bind1: S.symbol * binding,
	     bind2: S.symbol * binding) =
    case (bind1,bind2)
     of ((n1,FIXbind _),(n2,FIXbind _)) => S.symbolGt(n1,n2)
      | ((_,FIXbind _),_) => true
      | (_,(_,FIXbind _)) => false
      | ((n1,VALbind _),(n2,VALbind _)) => S.symbolGt(n1,n2)
      | ((_,VALbind _),_) => true
      | (_,(_,VALbind _)) => false
      | ((n1,CONbind _),(n2,CONbind _)) => S.symbolGt(n1,n2)
      | ((_,CONbind _),_) => true
      | (_,(_,CONbind _)) => false
      | ((n1,TYCbind _),(n2,TYCbind _)) => S.symbolGt(n1,n2)
      | ((_,TYCbind _),_) => true
      | (_,(_,TYCbind _)) => false
      | ((n1,STRbind _),(n2,STRbind _)) => S.symbolGt(n1,n2)
      | ((_,STRbind _),_) => true
      | (_,(_,STRbind _)) => false
      | ((n1,FCTbind _),(n2,FCTbind _)) => S.symbolGt(n1,n2)
      | ((_,FCTbind _),_) => true
      | (_,(_,FCTbind _)) => false
      | ((n1,SIGbind _),(n2,SIGbind _)) => S.symbolGt(n1,n2)
      | ((_,SIGbind _),_) => true
      | (_,(_,SIGbind _)) => false
      | ((n1,FSGbind _), (n2,FSGbind _)) => S.symbolGt(n1,n2)

end (* local *)
end (* structure Bindings *)

(*
 * $Log$
 *)
