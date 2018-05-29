(* mccommon.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* TODO: this module requires a signature ! *)

structure MCCommon =
struct

local structure EM = ErrorMsg
      structure DA = Access
      structure LV = LambdaVar
      open Types VarCon PLambda Absyn
in

type dconinfo = datacon * tyvar list

datatype pcon
  = DATApcon of dconinfo
  | INTpcon of int IntConst.t
  | WORDpcon of int IntConst.t
  | STRINGpcon of string
  | VLENpcon of int * ty

datatype path
  = RECORDPATH of path list
  | PIPATH of int * path
  | VPIPATH of int * ty * path
  | VLENPATH of ty * path
  | DELTAPATH of pcon * path
  | ROOTPATH

datatype dectree
  = CASETEST of
      path
      * Access.consig
      * (pcon * dectree) list
      * dectree option
  | ABSTEST0 of path * dconinfo * dectree * dectree
  | ABSTEST1 of path * dconinfo * dectree * dectree
  | RHS of int
  | BIND of path * dectree

type ruleno = int   (* the number identifying a rule *)
type rules = ruleno list
   (* a list of rule numbers in strictly ascending order without dups *)
(* Which occurrences of the int list type in the andor and decision definitions
 * are rules? Conjecture: all are. *)

datatype andor
  = AND of
     {bindings : (int * var) list,
      subtrees : andor list,
      constraints : (dconinfo * rules * andor option) list}
  | CASE of
     {bindings : (int * var) list,
      sign : DA.consig,
      cases : (pcon * rules * andor list) list,
      constraints : (dconinfo * rules * andor option) list}
   | LEAF of
     {bindings : (int * var) list,
      constraints : (dconinfo * rules * andor option) list}

datatype decision
  = CASEDEC of path
	     * DA.consig
             * (pcon * rules * decision list) list
	     * int list
  | ABSCONDEC of path * dconinfo * rules * decision list * int list
  | BINDDEC of path * rules


fun bug s = EM.impossible ("MCCommon: " ^ s)

fun mkRECORDpat (RECORDpat{fields, flex=false, typ, ...}) pats =
      RECORDpat {flex=false, typ=typ,
                 fields=ListPair.map(fn((id,_),p)=>(id,p))(fields,pats)}
  | mkRECORDpat (RECORDpat{flex=true,...}) _ =
      bug "mkRECORDpat - flex record"
  | mkRECORDpat _ _ = bug "mkRECORDpat - non-record"

fun conEq(DATACON{rep=a1,...}: datacon, DATACON{rep=a2,...}: datacon) = (a1 = a2)

fun conEq'((DATACON{rep=a1,...},_) : dconinfo, (DATACON{rep=a2,...},_): dconinfo) =
    (a1 = a2)

fun constantEq (DATApcon (d1, _), DATApcon (d2, _)) = conEq(d1, d2)
  | constantEq (INTpcon n, INTpcon n') = (#ty n = #ty n') andalso (#ival n = #ival n')
  | constantEq (WORDpcon n, WORDpcon n') = (#ty n = #ty n') andalso (#ival n = #ival n')
  | constantEq (STRINGpcon s, STRINGpcon s') = s = s'
  | constantEq (VLENpcon (n, _), VLENpcon (n',_)) = n = n'
  | constantEq _ = false

fun pathEq(RECORDPATH(a::ar),RECORDPATH(b::br)) =
	pathEq(a,b) andalso pathEq(RECORDPATH ar, RECORDPATH br)
  | pathEq(RECORDPATH nil, RECORDPATH nil) = true
  | pathEq(PIPATH(i1,p1),PIPATH(i2,p2)) = i1=i2 andalso pathEq(p1,p2)
  | pathEq(VPIPATH(i1,_,p1),VPIPATH(i2,_,p2)) = i1=i2 andalso pathEq(p1,p2)
  | pathEq(VLENPATH(_, p1),VLENPATH(_, p2)) = pathEq(p1,p2)
  | pathEq(DELTAPATH(c1,p1),DELTAPATH(c2,p2)) =
	               constantEq(c1,c2) andalso pathEq(p1,p2)
  | pathEq(ROOTPATH,ROOTPATH) = true
  | pathEq _ = false

type lvarEnv = (path * LV.lvar) list
fun lookupPath (a: path, (b,c)::d : lvarEnv) : LV.lvar =
       if pathEq(a,b) then c else lookupPath(a, d)
  | lookupPath _ = bug "lookupPath nil 2nd arg"

fun abstract x = false
fun template x = false
fun isAnException x = false
fun signOfCon (DATACON{sign,...}) = sign
fun unary (DATACON{const,...},_) = const

end (* toplevel local *)
end (* structure MCCommon *)
