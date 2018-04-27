(* mccommon.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* TODO: this module requires a signature ! *)

structure MCCommon =
struct

local structure EM = ErrorMsg
      open Types VarCon PLambda Absyn

in

type dconinfo = datacon * tyvar list

datatype pcon
  = DATApcon of dconinfo
  | INTpcon of int
  | INT32pcon of Int32.int
  | INTINFpcon of IntInf.int
  | WORDpcon of word
  | WORD32pcon of Word32.word
  | STRINGpcon of string
  | VLENpcon of int * ty

datatype path
  = RECORDPATH of path list
  | PIPATH of int * path
  | VPIPATH of int * ty * path
  | VLENPATH of path * ty
  | DELTAPATH of pcon * path
  | ROOTPATH

datatype dectree
  = CASETEST of
      path * Access.consig * (pcon * dectree) list
           * dectree option
  | ABSTEST0 of path * dconinfo * dectree * dectree
  | ABSTEST1 of path * dconinfo * dectree * dectree
  | RHS of int
  | BIND of path * dectree

fun bug s = EM.impossible ("MCCommon: " ^ s)

fun mkRECORDpat (RECORDpat{fields, flex=false, typ, ...}) pats =
      RECORDpat {flex=false, typ=typ,
                 fields=ListPair.map(fn((id,_),p)=>(id,p))(fields,pats)}
  | mkRECORDpat (RECORDpat{flex=true,...}) _ =
      bug "flex record passed to mkRECORDpat"
  | mkRECORDpat _ _ = bug "non record passed to mkRECORDpat"

fun conEq(DATACON{rep=a1,...},DATACON{rep=a2,...}) = (a1 = a2)

fun conEq'((DATACON{rep=a1,...},_), (DATACON{rep=a2,...},_)) = (a1 = a2)

(*
fun constantEq (INTcon n, INTcon n') = n = n'
  | constantEq (WORDcon n, WORDcon n') = n = n'
  | constantEq (INT32con n, INT32con n') = n = n'
  | constantEq (WORD32con n, WORD32con n') = n = n'
  | constantEq (STRINGcon s, STRINGcon s') = s = s'
  | constantEq (VLENcon n, VLENcon n') = n = n'
  | constantEq (DATAcon(_,krep,_), DATAcon(_,krep',_)) = krep = krep'
  | constantEq _ = false
*)

fun constantEq (DATApcon (d1, _), DATApcon (d2, _)) = conEq(d1, d2)
  | constantEq (INTpcon n, INTpcon n') = n = n'
  | constantEq (INT32pcon n, INT32pcon n') = n = n'
  | constantEq (INTINFpcon n, INTINFpcon n') = n = n'
  | constantEq (WORDpcon n, WORDpcon n') = n = n'
  | constantEq (WORD32pcon n, WORD32pcon n') = n = n'
  | constantEq (STRINGpcon s, STRINGpcon s') = s = s'
  | constantEq (VLENpcon (n, _), VLENpcon (n',_)) = n = n'
  | constantEq _ = false


fun pathEq(RECORDPATH(a::ar),RECORDPATH(b::br)) =
	pathEq(a,b) andalso pathEq(RECORDPATH ar, RECORDPATH br)
  | pathEq(RECORDPATH nil, RECORDPATH nil) = true
  | pathEq(PIPATH(i1,p1),PIPATH(i2,p2)) = i1=i2 andalso pathEq(p1,p2)
  | pathEq(VPIPATH(i1,_,p1),VPIPATH(i2,_,p2)) = i1=i2 andalso pathEq(p1,p2)
  | pathEq(VLENPATH(p1, _),VLENPATH(p2,_)) = pathEq(p1,p2)
  | pathEq(DELTAPATH(c1,p1),DELTAPATH(c2,p2)) =
	               constantEq(c1,c2) andalso pathEq(p1,p2)
  | pathEq(ROOTPATH,ROOTPATH) = true
  | pathEq _ = false

fun lookupPath (a, (b,c)::d) =
       if pathEq(a,b) then c else lookupPath(a, d)
  | lookupPath _ = bug "unexpected args in lookupPath"

fun abstract x = false
fun template x = false
fun isAnException x = false
fun signOfCon (DATACON{sign,...}) = sign
fun unary (DATACON{const,...},_) = const

end (* toplevel local *)
end (* structure MCCommon *)
