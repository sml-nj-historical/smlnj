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
fun binderGt ((s1, rb1), (s2, rb2)) = let
    (* hopefully the following gets optimized into an identity function
     * on tags... *)
    fun bnum (VALbind _) = 0
      | bnum (CONbind _) = 1
      | bnum (TYCbind _) = 2
      | bnum (SIGbind _) = 3
      | bnum (STRbind _) = 4
      | bnum (FSGbind _) = 5
      | bnum (FCTbind _) = 6
      | bnum (FIXbind _) = 7
in
    case Int.compare (bnum rb1, bnum rb2) of
	EQUAL => S.symbolGt (s1, s2)
      | GREATER => true
      | LESS => false
end

end (* local *)
end (* structure Bindings *)
