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
  | eqKind (TK_FUN (paramks, retknd), TK_FUN (paramks', retknd')) =
      (List.all eqKind (ListPair.zip (paramks, paramks'))) andalso eqKind(retknd,retknd')
 *)

(* lty.sml has a tk_eq "pointer" equality on normalized tkind *)
(* ltyextern.sml has a tkSubkind and tksSubkind functions *)

(* ltyextern.sml has a tkTycGen() that returns a function that returns the 
   kind of a given tyc using a given kenv *)
fun chkKind(tyc : tyc, kenv) =
    (case (tc_outX tyc) of
	 TC_VAR(n,argnum) => lookupKind(kenv, n, argnum)
       | TC_NVAR(lv) => raise Fail "Unimplemented" (* ... *)
       | TC_PRIM(ptyc) => 
