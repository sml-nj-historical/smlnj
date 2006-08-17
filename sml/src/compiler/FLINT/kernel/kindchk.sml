structure KindChk =
struct

structure LT = LtyExtern
open Lty

datatype frame = Beta of int * tyc list * tkind list (* suspension from beta red, r1 *)
	       | Lam of int * tkind list (* lifted lambda at given nesting level, r10 *)
withtype env = frame list

(* 
fun eqKind (TK_MONO, TK_MONO) = true
  | eqKind (TK_BOX, TK_BOX) = true
  | eqKind (TK_SEQ ks, TK_SEQ ks') = List.all eqKind (ListPair.zip (ks,ks'))
  | eqKind (TK_FUN (paramks, rngknd), TK_FUN (paramks', rngknd')) =
      (List.all eqKind (ListPair.zip (paramks, paramks'))) andalso eqKind(rngknd,rngknd')
 *)

(* lty.sml has a tk_eq "pointer" equality on normalized tkind *)
(* ltyextern.sml has a tkSubkind and tksSubkind functions *)


(* ltyextern.sml has a tkTycGen() that returns a function that returns the 
   kind of a given tyc using a given kenv *)
val tkTyc = tkTycGen()

exception KindCheck of string
fun error e = raise KindCheck e

fun chkKind(tyc: tyc, kenv) =
    let val chkKind' = fn tyc => chkKind(tyc, kenv)
    in
      (case (tc_outX tyc) of
	   TC_VAR(n,argnum) => lookupKind(kenv, n, argnum)
	 | TC_NVAR(lv) => raise Fail "Unimplemented" (* ... *)
	 | TC_PRIM(ptyc) => 
	 | TC_FN(paramknds, bodyTyc) => 
	   F(paramknds, 
	     chkKind(bodyTyc, tkInsert(kenv, paramknds)))
	 | TC_APP(opTyc, argTycs) =>
	   let 
	       val argKnds = map chkKind' argTycs
	   in
	       (case chkKind opTyc of
		    F(paramknds, rngknd) => 
		    if LT.tksSubkind(argKnds, paramknds)
		    then rngknd
		    else error "Arg/param kind mismatch"
		  | _ => error "Application of a non type \ 
			       \ function")
	   end
	 | TC_SEQ tycs => TK_SEQ(map chkKind' tycs)
	 | TC_PROJ (tyc, ind) => 
	     (case chkKind' tyc of
		  TK_SEQ ks => 
		    (List.nth ks ind 
		     handle Subscript => error "PROJ bad index")
		| _ => error "PROJ non-seq")
	 | TC_SUM
				 
