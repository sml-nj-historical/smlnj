(* COPYRIGHT (c) 1998 YALE FLINT PROJECT *)
(* equal.sml *)

signature EQUAL = 
sig

  (* 
   * Constructing generic equality functions; the current version will
   * use runtime polyequal function to deal with abstract types. (ZHONG)
   *)
  val equal_branch : FLINT.primop * FLINT.value list * FLINT.lexp * FLINT.lexp
                     -> FLINT.lexp
  val debugging : bool ref     

end (* signature EQUAL *)


structure Equal : EQUAL = 
struct

local structure BT = BasicTypes
      structure LT = LtyExtern
      structure PT = PrimTyc
      structure PO = PrimOp
      structure PP = PrettyPrint
      structure FU = FlintUtil
      open FLINT
in

val debugging = ref false
fun bug msg = ErrorMsg.impossible("Equal: "^msg)
val say = Control.Print.say
val mkv = LambdaVar.mkLvar
val ident = fn x => x


val (trueDcon', falseDcon') = 
  let val lt = LT.ltc_arrow(LT.ffc_rrflint, [LT.ltc_unit], [LT.ltc_bool])
      fun h (Types.DATACON{name, rep, ...}) = (name, rep, lt)
   in (h BT.trueDcon, h BT.falseDcon)
  end

val tcEqv = LT.tc_eqv


fun boolLexp b = 
  let val v = mkv() and w = mkv()
      val dc = if b then trueDcon' else falseDcon'
   in RECORD(FU.rk_tuple, [], v, CON(dc, [], VAR v, w, RET[VAR w]))
  end

fun trueLexp () = boolLexp true
fun falseLexp () = boolLexp false

exception Poly

(****************************************************************************
 *                   Commonly-used Lambda Types                             *
 ****************************************************************************)

(** assumptions: typed created here will be reprocessed in wrapping.sml *)
fun eqLty lt  = LT.ltc_arrow(LT.ffc_rrflint, [lt, lt], [LT.ltc_bool])
fun eqTy tc   = eqLty(LT.ltc_tyc tc)

val inteqty   = eqLty (LT.ltc_int)
val int32eqty = eqLty (LT.ltc_int32)
val booleqty  = eqLty (LT.ltc_bool)
val realeqty  = eqLty (LT.ltc_real)

datatype resKind 
  = VBIND of value
  | PBIND of primop
  | EBIND of lexp

fun ptrEq(p, tc) = PBIND (NONE, p, eqTy tc, [])
fun prim(p, lt) = PBIND (NONE, p, lt, [])

fun branch(PBIND p, vs, e1, e2) = BRANCH(p, vs, e1, e2)
  | branch(VBIND v, vs, e1, e2) = 
      let val x = mkv()
       in LET([x], APP(v, vs), 
            SWITCH(VAR x, BT.boolsign, 
                   [(DATAcon(trueDcon', [], mkv()), e1),
                    (DATAcon(falseDcon', [], mkv()), e2)], NONE))
      end
  | branch(EBIND e, vs, e1, e2) = 
      let val x = mkv()
       in LET([x], e, branch(VBIND (VAR x), vs, e1, e2))
      end

(****************************************************************************
 *              equal --- the equality function generator                   *
 ****************************************************************************)
exception Notfound

fun equal (peqv, seqv, tc) = 
let 

val cache : (tyc * lvar * (fundec option ref)) list ref = ref nil
                   (* lexp ref is used for recursions ? *)

fun enter tc =
  let val v = mkv()
      val r = ref NONE
   in cache := (tc, v, r) :: !cache; (v, r)
  end
  (* the order of cache is relevant; the hdr may use the tail *)

fun find tc =
  let fun f ((t,v,e)::r) = if tcEqv(tc,t) then VBIND(VAR v) else f r
        | f [] = (if !debugging
                  then say "equal.sml-find-notfound\n" else ();
                  raise Notfound)
   in f (!cache)
  end

fun atomeq tc =
  if tcEqv(tc,LT.tcc_int) then prim(PO.IEQL,inteqty)
  else if tcEqv(tc,LT.tcc_int32) then prim(PO.IEQL,int32eqty)
  else if tcEqv(tc,LT.tcc_bool) then prim(PO.IEQL,booleqty) 
  else if tcEqv(tc,LT.tcc_string) then VBIND (VAR seqv)
  else if (LT.tcp_app tc)
    then let
      val (x, _) = LT.tcd_app tc
      in
	if ((LT.tcp_prim x) andalso (LT.tcd_prim x = PT.ptc_ref))
	  then ptrEq(PO.PTREQL, tc) (* 'a ref *)
(** SOMEDAY
	else if ((LT.tcp_prim x) andalso (LT.tcd_prim x = PT.ptc_array))
	  then ???
**)
	  else raise Poly
      end
  else raise Poly

val fkfun = {isrec=NONE, known=false, cconv=CC_FUN LT.ffc_rrflint, inline=IH_ALWAYS}

fun test(tc, 0) = raise Poly
  | test(tc, depth) =
     if LT.tcp_tuple tc then
       (let val ts = LT.tcd_tuple tc       
         in (find tc handle Notfound =>
              let val x=mkv() and y=mkv()
                  val (v, patch) = enter tc 

                  fun loop(n, tx::r) = 
                        let val a = mkv() and b = mkv()
                         in SELECT(VAR x, n, a, 
                              SELECT(VAR y, n, b,
                                branch(test(tx, depth), [VAR a, VAR b],
                                       loop(n+1, r), falseLexp())))
                        end
                    | loop(_, []) = trueLexp()

                  val lt = LT.ltc_tyc tc
               in patch := SOME (fkfun, v, [(x, lt), (y, lt)], loop(0, ts));
                  VBIND(VAR v)
              end)
        end)
     else atomeq tc

val body = test(tc, 10)
val fl = !cache

in 
(case fl 
  of [] => body
   | _ => let fun g ((tc, f, store), e) = 
                (case !store 
                  of NONE => e
                   | SOME fd => FIX([fd], e))
           in case body
               of PBIND _ => bug "unexpected PBIND in equal"
                | VBIND u => EBIND(foldr g (RET[u]) fl)
                | EBIND e => EBIND(foldr g e fl)
          end)

end handle Poly => EBIND(TAPP(VAR peqv, [tc]))


fun equal_branch ((d, p, lt, ts), vs, e1, e2) = 
  (case (d, p, ts)
    of (SOME{default=pv, table=[(_,sv)]}, PO.POLYEQL, [tc]) =>
          branch(equal(pv, sv, tc), vs, e1, e2)
     | _ => bug "unexpected case in equal_branch")

end (* toplevel local *)                       
end (* structure Equal *)


(*
 * $Log: equal.sml,v $
 * Revision 1.3  1998/12/22 17:02:02  jhr
 *   Merged in 110.10 changes from Yale.
 *
 * Revision 1.1.1.1  1998/04/08 18:39:44  george
 * Version 110.5
 *
 *)
