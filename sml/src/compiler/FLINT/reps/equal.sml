(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* equal.sml *)

signature EQUAL = 
sig

  (* 
   * Constructing generic equality functions; the current version will
   * use runtime polyequal function to deal with abstract types. (ZHONG)
   *)
  val equal : LambdaVar.lvar * LambdaVar.lvar * LtyDef.tyc -> Lambda.lexp
  val debugging : bool ref     

end (* signature EQUAL *)


structure Equal : EQUAL = 
struct

local structure DA = Access
      structure BT = BasicTypes
      structure LT = LtyExtern
      structure PT = PrimTyc
      structure PO = PrimOp
      structure PP = PrettyPrint
      open Lambda 
in

val debugging = ref false
fun bug msg = ErrorMsg.impossible("Equal: "^msg)
val say = Control.Print.say

val (trueDcon', falseDcon') = 
  let val lt = LT.ltc_parrow(LT.ltc_unit, LT.ltc_bool)
      fun h (Types.DATACON{name, rep, ...}) = (name, rep, lt)
   in (h BT.trueDcon, h BT.falseDcon)
  end

val tcEqv = LT.tc_eqv

(*
 * MAJOR CLEANUP REQUIRED ! The function mkv is currently directly taken 
 * from the LambdaVar module; I think it should be taken from the 
 * "compInfo". Similarly, should we replace all mkLvar in the backend
 * with the mkv in "compInfo" ? (ZHONG)
 *)
val mkv = LambdaVar.mkLvar

val ident = fn x => x
fun split(SVAL v) = (v, ident)
  | split x = let val v = mkv()
               in (VAR v, fn z => LET(v, x, z))
              end

fun APPg(e1, e2) = 
  let val (v1, h1) = split e1
      val (v2, h2) = split e2
   in h1(h2(APP(v1, v2)))
  end

fun RECORDg es = 
  let fun f ([], vs, hdr) = hdr(RECORD (rev vs))
        | f (e::r, vs, hdr) = 
              let val (v, h) = split e
               in f(r, v::vs, hdr o h)
              end
   in f(es, [], ident)
  end

fun SWITCHg(e, csig, ces, oe) =
  let val (v, h) = split e
   in h(SWITCH(v, csig, ces, oe))
  end

fun CONg(dc, ts, e) = 
  let val (v, h) = split e
   in h(CON(dc, ts, v))
  end

val (trueLexp, falseLexp) =
  let val unitLexp = RECORD []
   in (CONg (trueDcon', [], unitLexp), CONg (falseDcon', [], unitLexp))
  end

exception Poly

(****************************************************************************
 *                   Commonly-used Lambda Types                             *
 ****************************************************************************)

val boolty = LT.ltc_bool
fun eqLty lt = LT.ltc_arw(LT.ltc_tuple [lt, lt], boolty)
val inteqty = eqLty (LT.ltc_int)
val int32eqty = eqLty (LT.ltc_int32)
val booleqty = eqLty (LT.ltc_bool)
val realeqty = eqLty (LT.ltc_real)

fun eqTy tc = eqLty(LT.ltc_tyc tc)
fun ptrEq(p, tc) = PRIM(p, eqTy tc, [])
fun prim(p, lt) = PRIM(p, lt, [])

fun isRef tc = 
  if LT.tcp_app tc then
    (let val (x, _) = LT.tcd_app tc
      in if LT.tcp_prim x 
         then (let val pt = LT.tcd_prim x
                in (pt = PT.ptc_ref) orelse (pt = PT.ptc_array)
               end)
         else false
     end)
  else false

exception Notfound

(****************************************************************************
 *              equal --- the equality function generator                   *
 ****************************************************************************)
fun equal (peqv, seqv, tc) = 
let val cache : (tyc * lvar * lexp ref) list ref = ref nil

fun enter tc =
  let val v = mkv()
      val r = ref (SVAL(VAR v))
   in cache := (tc, v, r) :: !cache; (VAR v, r)
  end

fun find tc =
  let fun f ((t,v,e)::r) = if tcEqv(tc,t) then VAR v else f r
        | f [] = (if !debugging
                  then say "equal.sml-find-notfound\n" else ();
                  raise Notfound)
   in f (!cache)
  end

fun atomeq tc =
  if tcEqv(tc,LT.tcc_int) then prim(PO.IEQL,inteqty)
  else if tcEqv(tc,LT.tcc_int32) then prim(PO.IEQL,int32eqty)
  else if tcEqv(tc,LT.tcc_bool) then prim(PO.IEQL,booleqty) 
  else if tcEqv(tc,LT.tcc_real) then prim(PO.FEQLd,realeqty)
  else if tcEqv(tc,LT.tcc_string) then (VAR seqv)
  else if isRef(tc) then ptrEq(PO.PTREQL, tc)
  else raise Poly

fun test(tc, 0) = raise Poly
  | test(tc, depth) =
     if LT.tcp_tuple tc then
       (let val ts = LT.tcd_tuple tc       
         in (find tc handle Notfound =>
              let val v = mkv() and x=mkv() and y=mkv()
                  val (eqv, patch) = enter tc
                   fun loop(n, [tx]) = 
                         APPg(SVAL (test(tx, depth)), 
                             RECORDg[SELECT(n, VAR x),
                                     SELECT(n, VAR y)])

                     | loop(n, tx::r) = 
                         SWITCHg(loop(n,[tx]), BT.boolsign,
                                [(DATAcon(trueDcon'), loop(n+1,r)),
                                 (DATAcon(falseDcon'), falseLexp)],
                                NONE)

                     | loop(_,nil) = trueLexp

                   val lt = LT.ltc_tyc tc
                in patch := FN(v, LT.ltc_tuple [lt,lt],
                             LET(x, SELECT(0, VAR v),
                               LET(y, SELECT(1, VAR v), 
                                    loop(0, ts))));
                   eqv
               end)
         end)
      else atomeq tc

val body = SVAL(test(tc, 10))
val fl = !cache

in 
(case fl 
  of [] => body
   | _ => let fun g ((tc, v, e), (vs, ts, es)) = 
                       (v::vs, (eqTy tc)::ts, (!e)::es)
              val (vs, ts, es) = foldr g ([], [], []) fl
           in FIX(vs, ts, es, body)
          end)
end handle Poly => (TAPP(VAR peqv, [tc]))

end (* toplevel local *)                       
end (* structure Equal *)

