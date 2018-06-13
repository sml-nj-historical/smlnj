(* equal.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

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
      structure PO = Primop
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

exception Poly

(****************************************************************************
 *                   Commonly-used FLINT Types                              *
 ****************************************************************************)

(** assumptions: typed created here will be reprocessed in wrapping.sml *)
fun eqLty lt  = LT.ltc_arrow(LT.ffc_rrflint, [lt, lt], [LT.ltc_bool])
fun eqTy tc   = eqLty(LT.ltc_tyc tc)

val inteqty   = eqLty (LT.ltc_int)
val int32eqty = eqLty (LT.ltc_num 32)	(* 64BIT: FIXME *)
val booleqty  = eqLty (LT.ltc_bool)
val realeqty  = eqLty (LT.ltc_real)	(* REAL32: FIXME *)

(****************************************************************************
 *              equal --- the equality function generator                   *
 ****************************************************************************)
exception Notfound

val fkfun = {isrec=NONE, known=false, cconv=CC_FUN LT.ffc_rrflint, inline=IH_SAFE}

fun branch (e, te, fe) =
    let val x = mkv()
    in LET([x], e,
	   SWITCH(VAR x, BT.boolsign,
		  [(DATAcon(trueDcon', [], mkv()), te),
		   (DATAcon(falseDcon', [], mkv()), fe)], NONE))
    end

fun equal (peqv, seqv) = let

    fun eq (tc, x, y, 0, te, fe) = raise Poly
      | eq (tc, x, y, d, te, fe) = let

	fun eq_tuple (_, _, [], te, fe) = te
	  | eq_tuple (n, d, ty::tys, te, fe) =
            let val a = mkv()
		val b = mkv()
            in SELECT(x, n, a,
                      SELECT(y, n, b,
                             eq(ty, VAR a, VAR b, d - 1,
				eq_tuple(n + 1, d - 1, tys, te, fe),
				fe)))
            end

    in
	if LT.tcp_tuple tc then
	    case fe of (APP _ | RET _) =>
		       eq_tuple(0, d, LT.tcd_tuple tc, te, fe)
		     | _ =>
		       let val f = mkv()
		       in FIX([(fkfun, f, [], fe)],
			      eq_tuple(0, d, LT.tcd_tuple tc,
				       te, APP(VAR f, [])))
		       end
	else if tcEqv(tc,LT.tcc_int) then
	    BRANCH((NONE, PO.IEQL, inteqty, []), [x,y], te, fe)
(* 64BIT: FIXME *)
	else if tcEqv(tc,LT.tcc_num 32) then
	    BRANCH((NONE, PO.IEQL, int32eqty, []), [x,y], te, fe)
	else if tcEqv(tc,LT.tcc_bool) then
	    BRANCH((NONE, PO.IEQL, booleqty, []), [x,y], te, fe)
	else if tcEqv(tc,LT.tcc_string) then
	    branch(APP(VAR seqv, [x,y]), te, fe)
	else if (LT.tcp_app tc) andalso
		let val (x, _) = LT.tcd_app tc
		in ((LT.tcp_prim x) andalso (LT.tcd_prim x = PT.ptc_ref))
		end then
	    BRANCH((NONE, PO.PTREQL, eqTy tc, []), [x,y], te, fe)
	else raise Poly
    end

in (fn (tc,x,y,d,te,fe) => eq (tc,x,y,d,te,fe)
       handle Poly =>
	      let val f = mkv()
	      in LET([f], TAPP(VAR peqv, [tc]),
		     branch(APP(VAR f, [x,y]), te, fe))
	      end)
end

fun equal_branch ((d, p, lt, ts), vs, e1, e2) =
  (case (d, p, ts, vs)
    of (SOME{default=pv, table=[(_,sv)]}, PO.POLYEQL, [tc], [x, y]) =>
          equal (pv, sv) (tc, x, y, 10, e1, e2)
     | _ => bug "unexpected case in equal_branch")

end (* toplevel local *)
end (* structure Equal *)
