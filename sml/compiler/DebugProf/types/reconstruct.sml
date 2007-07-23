(* COPYRIGHT (c) 1996 AT&T Bell Laboratories *)
(* reconstruct.sml *)

structure Reconstruct : sig val expType : Absyn.exp -> Types.ty end =
struct

structure TU = TypesUtil
open Absyn (* ElabUtil *) VarCon Types CoreBasicTypes TypesUtil

fun bug msg = ErrorMsg.impossible("Reconstruct: "^msg)

infix -->

fun reduceType(POLYty{tyfun=TYFUN{body,arity},...}) = headReduceType body
  | reduceType ty = headReduceType ty

fun expType(VARexp(ref(VALvar{typ=ref ty,...}),insttvs)) =
     (case ty
	  of POLYty{tyfun,...} => TU.applyTyfun(tyfun,map VARty insttvs)
	   | _ => ty)
  | expType(VARexp _) = bug "varexp"
  | expType(CONexp(DATACON{typ,...},insttvs)) =
     (case typ
	  of POLYty{tyfun,...} => TU.applyTyfun(tyfun,map VARty insttvs)
	   | _ => typ)
  | expType(INTexp (_, t)) = t
  | expType(WORDexp (_, t)) = t
  | expType(STRINGexp _) = stringTy
  | expType(CHARexp _) = charTy
  | expType(REALexp _) = realTy
  | expType(RECORDexp fields) =
	let fun extract(LABEL{name,...},exp) = (name,expType exp)
	 in recordTy(map extract (sortFields fields))
	end
  | expType(VECTORexp(nil,vty)) = CONty(vectorTycon,[vty])
  | expType(VECTORexp((a::_),vty)) = CONty(vectorTycon,[vty])
  (*| expType(PACKexp(e, t, _)) = t*)
  | expType(SEQexp [a]) = expType a
  | expType(SEQexp (_::rest)) = expType(SEQexp rest)
  | expType(APPexp(rator,rand)) =
	(case reduceType(expType rator)
	  of CONty(_,[_,t]) => t
	   | POLYty _ => bug "poly-rator"
	   | WILDCARDty => bug "wildcard-rator"
	   | UNDEFty => bug "undef-rator"
	   | IBOUND _ => bug "ibound-rator" 
	   | VARty _ => bug "varty-rator"
	   | _ => bug "rator")
  | expType(CONSTRAINTexp(e,ty)) = expType e
  | expType(HANDLEexp(e,h)) = expType e
  | expType(RAISEexp(e,t)) = t
  | expType(LETexp(_,e)) = expType e
  | expType(CASEexp(_,RULE(_,e)::_,_)) = expType e
  | expType(FNexp(RULE(_,e)::_, ty)) = ty --> expType e
  | expType(MARKexp(e,_)) = expType e
  | expType _ = bug "expType"
 
end (* structure Reconstruct *)


