(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* pequal.sml *)

signature PEQUAL = 
sig
  type toTcLt = (Types.ty -> PLambdaType.tyc) * (Types.ty -> PLambdaType.lty)
  (* 
   * Constructing generic equality functions; the current version will
   * use runtime polyequal function to deal with abstract types. (ZHONG)
   *)
  val equal : {getStrEq : unit -> PLambda.lexp, 
               getPolyEq : unit -> PLambda.lexp} * StaticEnv.staticEnv 
               -> (Types.ty * Types.ty * toTcLt) -> PLambda.lexp

  val debugging : bool ref     

end (* signature PEQUAL *)


structure PEqual : PEQUAL = 
struct

local structure DA = Access
      structure EM = ErrorMsg
      structure T  = Types
      structure BT = BasicTypes
      structure LT = PLambdaType
      structure TU = TypesUtil
      structure SE = StaticEnv
      structure PO = PrimOp
      structure PP = PrettyPrint
      open Types PLambda 

in

val debugging = ref false
fun bug msg = ErrorMsg.impossible("Equal: "^msg)
val say = Control.Print.say

type toTcLt = (ty -> LT.tyc) * (ty -> LT.lty)

val --> = BT.-->
infix -->

(*
 * MAJOR CLEANUP REQUIRED ! The function mkv is currently directly taken 
 * from the LambdaVar module; I think it should be taken from the 
 * "compInfo". Similarly, should we replace all mkLvar in the backend
 * with the mkv in "compInfo" ? (ZHONG)
 *)
val mkv = LambdaVar.mkLvar

(** translating the typ field in DATACON into lty; constant datacons 
    will take ltc_unit as the argument *)
fun toDconLty (toTyc, toLty) ty =
  (case ty 
    of POLYty{sign, tyfun=TYFUN{arity, body}} =>
         if BT.isArrowType body then toLty ty
         else toLty (POLYty{sign=sign, 
                            tyfun=TYFUN{arity=arity,
                                        body=BT.-->(BT.unitTy, body)}})
     | _ => if BT.isArrowType ty then toLty ty
            else toLty (BT.-->(BT.unitTy, ty)))

(* 
 * Is TU.dconType necessary, or could a variant of transTyLty that 
 * just takes tyc and domain be used in transDcon??? 
 *)
fun transDcon(tyc, {name,rep,domain}, toTcLt) =
      (name, rep, toDconLty toTcLt (TU.dconType(tyc,domain)))

val (trueDcon', falseDcon') = 
  let val lt = LT.ltc_parrow(LT.ltc_unit, LT.ltc_bool)
      fun h (DATACON{name, rep, ...}) = (name, rep, lt)
   in (h BT.trueDcon, h BT.falseDcon)
  end

fun COND(a,b,c) =
  SWITCH(a, BT.boolsign, [(DATAcon(trueDcon', [], mkv()),b),
                          (DATAcon(falseDcon', [], mkv()),c)], NONE)

val (trueLexp, falseLexp) =
  let val unitLexp = RECORD []
   in (CON (trueDcon', [], unitLexp), CON (falseDcon', [], unitLexp))
  end

fun argType(domain, []) = domain
  | argType(domain, args) =
      TU.applyTyfun(TYFUN{arity=length args,body=domain},args)

fun reduceTy ty =
  (case TU.headReduceType ty
    of POLYty{tyfun=TYFUN{body,...},...} => reduceTy body
     | ty => ty)

fun expandREC (family as {members: T.dtmember vector, ...}, stamps, freetycs) =
  let fun g (RECtyc i) = 
           let val {tycname,dcons,arity,eq,lazyp,sign} =
	           Vector.sub(members,i)
               val s = Vector.sub(stamps, i)
            in GENtyc{stamp=s,arity=arity,eq=ref(YES), 
		      kind=DATATYPE{index=i, family=family,root=NONE,
				    stamps=stamps, freetycs=freetycs},
		      path=InvPath.IPATH[tycname],
		      stub = NONE}
	   end
        | g (FREEtyc i) = List.nth(freetycs, i)
        | g x = x
      fun f(CONty(tyc,tyl)) = CONty(g tyc, map f tyl)
        | f(x as IBOUND _) = x
        | f _ = bug "unexpected type in expandREC"
   in f
  end

exception Poly

fun equivType(ty,ty') =
  let fun eq(ty as CONty(tycon, args), ty' as CONty(tycon', args')) =
              (if TU.eqTycon(tycon, tycon')
	       then ListPair.all equivType (args,args') 
	       else (equivType(TU.reduceType ty, ty')
		     handle ReduceType =>
			 (equivType(ty,TU.reduceType ty')
 		    handle ReduceType => false)))
        | eq(VARty _, _) = raise Poly
        | eq(_, VARty _) = raise Poly
        | eq(POLYty _, _) = raise Poly
        | eq(_, POLYty _) = raise Poly
        | eq _ = false
   in eq(TU.prune ty, TU.prune ty')
  end

(****************************************************************************
 *                   Commonly-used Lambda Types                             *
 ****************************************************************************)

val boolty = LT.ltc_bool
fun eqLty lt = LT.ltc_parrow(LT.ltc_tuple [lt, lt], boolty)
val inteqty = eqLty (LT.ltc_int)
val int32eqty = eqLty (LT.ltc_int32)
val booleqty = eqLty (LT.ltc_bool)
val realeqty = eqLty (LT.ltc_real)

exception Notfound

(****************************************************************************
 *              equal --- the equality function generator                   *
 ****************************************************************************)
fun equal ({getStrEq, getPolyEq}, env) 
          (polyEqTy : ty, concreteType : ty, toTcLc as (toTyc, toLty)) =
let 

val cache : (ty * lexp * lexp ref) list ref = ref nil

fun enter ty =
  let val v = VAR(mkv())
      val r = ref v
   in if !debugging 
      then PP.with_pp (EM.defaultConsumer())
            (fn ppstrm => (PP.string ppstrm "enter: ";
               PPType.resetPPType(); PPType.ppType env ppstrm ty))
      else ();
      cache := (ty, v, r) :: !cache; (v,r)
  end

fun find ty =
  let fun f ((t,v,e)::r) = if equivType(ty,t) then v else f r
        | f [] = (if !debugging
                  then say "equal.sml-find-notfound\n" else ();
                  raise Notfound)
   in if !debugging 
      then PP.with_pp (EM.defaultConsumer())
           (fn ppstrm => (PP.string ppstrm "find: ";
                          PPType.resetPPType();
                          PPType.ppType env ppstrm ty))
      else ();
      f (!cache)
  end

fun eqTy ty = eqLty(toLty ty)
fun ptrEq(p, ty) = PRIM(p, eqTy ty, [])
fun prim(p, lt) = PRIM(p, lt, [])

fun atomeq (tyc, ty) =
  if TU.equalTycon(tyc,BT.intTycon) then prim(PO.IEQL,inteqty)
  else if TU.equalTycon(tyc,BT.int32Tycon) then prim(PO.IEQL,int32eqty)
  else if TU.equalTycon(tyc,BT.wordTycon) then prim(PO.IEQL,inteqty)
  else if TU.equalTycon(tyc,BT.word8Tycon) then prim(PO.IEQL,inteqty)
  else if TU.equalTycon(tyc,BT.charTycon) then prim(PO.IEQL,inteqty)
  else if TU.equalTycon(tyc,BT.word32Tycon) then prim(PO.IEQL,int32eqty)
  else if TU.equalTycon(tyc,BT.boolTycon) then prim(PO.IEQL,booleqty) 
  else if TU.equalTycon(tyc,BT.realTycon) then prim(PO.FEQLd,realeqty)
  else if TU.equalTycon(tyc,BT.stringTycon) then getStrEq()
  else if TU.equalTycon(tyc,BT.refTycon) then ptrEq(PO.PTREQL, ty) 
(**********************
 * For arrays under the new array representation, we need to compare
 * the data pointers for equality.  polyequal does this comparison
 * correctly, so use it as the fallback. (JHR)
 *
  else if TU.equalTycon(tyc,BT.arrayTycon) then ptrEq(PO.PTREQL, ty)
  else if TU.equalTycon(tyc,BT.word8arrayTycon) then ptrEq(PO.PTREQL, ty)
  else if TU.equalTycon(tyc,BT.real64arrayTycon) then ptrEq(PO.PTREQL, ty)
**********************)
  else raise Poly

fun test(ty, 0) = raise Poly
  | test(ty, depth) =
     (if !debugging
      then PP.with_pp (EM.defaultConsumer())
           (fn ppstrm => (PP.string ppstrm "test: ";
                          PPType.resetPPType();
                          PPType.ppType env ppstrm ty))
      else ();

      case ty
       of VARty(ref(INSTANTIATED t)) => test(t,depth)
        | CONty(DEFtyc _, _) => test(TU.reduceType ty,depth)
        | CONty(RECORDtyc _, tyl) =>
            (find ty handle Notfound =>
               let val v = mkv() and x=mkv() and y=mkv()
                   val (eqv, patch) = enter ty
                   fun loop(n, [ty]) = 
                         APP(test(ty,depth), RECORD[SELECT(n, VAR x),
                                                    SELECT(n, VAR y)])
                     | loop(n, ty::r) = 
                         COND(loop(n,[ty]), loop(n+1,r), falseLexp)
                     | loop(_,nil) = trueLexp

                   val lt = toLty ty
                in patch := FN(v, LT.ltc_tuple [lt,lt],
                             LET(x, SELECT(0, VAR v),
                               LET(y, SELECT(1, VAR v), 
                                    loop(0, tyl))));
                   eqv
               end)

	| CONty (tyc as GENtyc { kind, eq, stamp, arity, path, ... }, tyl) =>
	  (case (!eq, kind) of
	       (YES, PRIMITIVE _) => atomeq (tyc, ty)

             | (ABS,_) =>
               test(TU.mkCONty(GENtyc{eq=ref YES,stamp=stamp,arity=arity,
                                      kind=kind,path=path,stub=NONE}, tyl),
		    depth)
             (* assume that an equality datatype has been converted
	      * to an abstract type in an abstype declaration *)

             | (_,DATATYPE{index,family as {members,...},
                           freetycs,stamps,...}) =>
               let val {dcons=dcons0,...} = Vector.sub(members,index)
                   fun expandRECdcon{domain=SOME x, rep, name} = 
                       {domain=SOME(expandREC (family, stamps, freetycs) x),
			rep=rep,name=name}
                     | expandRECdcon z = z

               in
		   case map expandRECdcon dcons0
                    of [{rep=REF,...}] => atomeq(tyc, ty)
                     | dcons =>                          
                       (find ty
			handle Notfound => let
		          val v = mkv()
			  val x=mkv()
			  val y=mkv()
			  val (eqv, patch) = enter ty
			  fun inside ({name,rep,domain}, ww, uu) = 
			      case domain of
				  NONE => trueLexp
				| SOME dom => 
				  (case reduceTy dom
				    of (CONty(RECORDtyc [], _)) =>
				       trueLexp
				     | _ => let
					   val argt = argType(dom, tyl)
				       in
					   APP(test(argt, depth-1),
					       RECORD[VAR ww, VAR uu])
				       end)
			  val lt = toLty ty
			  val argty = LT.ltc_tuple [lt,lt]
			  val pty = LT.ltc_parrow(argty, boolty)
 
			  val body = 
			      case dcons
			       of [] => bug "empty data types"
(*                              | [dcon] => inside dcon       *)
				| _ => let
				      (* this is somewhat a hack !! *)
				      (* val sign = map #rep dcons *)
				      fun isConst(DA.CONSTANT _) =
					  true
					| isConst(DA.LISTNIL) = true
					| isConst _ = false

				      fun getCsig({rep=a,domain,name}::r,c,v)= 
					  if isConst a then getCsig(r, c+1, v)
					  else getCsig(r, c, v+1)
					| getCsig([], c, v) = DA.CSIG(v,c)

				      val sign = getCsig(dcons,0,0)

				      fun concase dcon = 
					  let val tcs = map toTyc tyl
					      val ww = mkv()
					      val uu = mkv()
					      val dc =
						  transDcon(tyc,dcon,toTcLc)
					      val dconx = DATAcon(dc, tcs, ww)
					      val dcony = DATAcon(dc, tcs, uu)
					  in
					      (dconx,
					       SWITCH(VAR y, sign, 
						      [(dcony,
							inside(dcon,ww,uu))],
						      SOME(falseLexp)))
					  end
				  in
				      SWITCH(VAR x, sign, 
					     map concase dcons, NONE)
				  end

                          val root = APP(PRIM(PO.PTREQL, pty, []), 
                                         RECORD[VAR x, VAR y])
                          val nbody = COND(root, trueLexp, body)
                      in
			  patch := FN(v, argty,
				      LET(x, SELECT(0, VAR v),
					  LET(y, SELECT(1, VAR v), nbody)));
			  eqv
		      end)
               end
	     | _ => raise Poly)
        | _ => raise Poly)

val body = test(concreteType, 10)
val fl = !cache

in 

(case fl 
  of [] => body
   | _ => let fun g ((ty, VAR v, e), (vs, ts, es)) = 
                        (v::vs, (eqTy ty)::ts, (!e)::es)
                | g _ = bug "unexpected equality cache value"

              val (vs, ts, es) = foldr g ([], [], []) fl
           in FIX(vs, ts, es, body)
          end)

end handle Poly => 
  (GENOP({default=getPolyEq(),
          table=[([LT.tcc_string], getStrEq())]}, 
         PO.POLYEQL, toLty polyEqTy, 
         [toTyc concreteType]))


end (* toplevel local *)                       
end (* structure Equal *)

