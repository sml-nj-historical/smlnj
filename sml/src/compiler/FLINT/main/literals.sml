(* COPYRIGHT (c) 1998 YALE FLINT PROJECT *)
(* literals.sml *)

signature LITERALS =
 sig
   type lit
   val litsplit : CPS.function -> CPS.function * lit
   val lit2cps : lit -> CPS.function
 end

structure Literals : LITERALS = 
struct

local structure LV = LambdaVar
      open CPS
in

fun bug msg = ErrorMsg.impossible ("Literals: "^msg) 
val ident = fn x => x
val liftLiterals = Control.CG.liftLiterals
fun mkv _ = LV.mkLvar()

(****************************************************************************
 *                         A MINI-LITERAL LANGUAGE                          *
 ****************************************************************************)
datatype lit_val
  = LI_INT of int
  | LI_INT32 of Word32.word
  | LI_REAL of string
  | LI_STRING of string
  | LI_VAR of lvar

datatype lit_exp
  = LI_TOP of lit_val list
  | LI_RECORD of record_kind * lit_val list * lvar * lit_exp

type lit = lit_exp

fun val2lit (VAR v) = LI_VAR v
  | val2lit (INT i) = LI_INT i
  | val2lit (INT32 i) = LI_INT32 i
  | val2lit (REAL s) = LI_REAL s
  | val2lit (STRING s) = LI_STRING s
  | val2lit _ = bug "unexpected case in val2lit"

(****************************************************************************
 *                 TRANSLATING THE LITERAL EXP TO CPS EXP                   *
 ****************************************************************************)
fun lit2cps li = 
  let val k = mkv()

      fun toval (LI_INT i) = INT i
        | toval (LI_INT32 i) = INT32 i
        | toval (LI_REAL s) = REAL s
        | toval (LI_STRING s) = STRING s
        | toval (LI_VAR v) = VAR v

      fun toexp (LI_TOP []) = APP(VAR k, [INT 0])
        | toexp (LI_TOP vs) = 
            let val v = mkv()
                val nvs = map (fn x => (toval x, OFFp 0)) vs
             in RECORD(RK_RECORD, nvs, v, APP(VAR k, [VAR v]))
            end
        | toexp (LI_RECORD (rk, vs, v, e)) = 
            let val nvs = map (fn x => (toval x, OFFp 0)) vs
             in RECORD(rk, nvs, v, toexp e)
            end

      val f = mkv()
      val x = mkv()
   in (ESCAPE, f, [k, x], [CNTt, BOGt], toexp li)
  end 


(****************************************************************************
 *                    LIFTING LITERALS ON FLINT                             *
 ****************************************************************************)
(*
fun liftlits body = bug "FLINT version currently not implemented yet"

fun litsplit (FK_FCT, f, [(v, t)], body) = 
      if LT.ltp_str t then
        let val (nbody, lit, llt) = 
              if !liftLiterals then liftlits body
              else (body, LI_TOP [], LT.ltc_str[])
            val nt = LT.ltc_str ((LT.ltd_str t)@[llt])
         in ((FK_FCT, f, [(v, nt)], body), lit)
        end
      else bug "unexpected FLINT header in litsplit (case 1)"
  | litsplit _ = bug "unexpected FLINT header in litsplit (case 2)"
*)

(****************************************************************************
 *                    LIFTING LITERALS ON CPS                               *
 ****************************************************************************)
datatype info 
  = ZZ_STR of string
  | ZZ_FLT of string
  | ZZ_RCD of record_kind * value list

exception LitInfo

datatype rlit = RLIT of string * int
fun toRlit s = RLIT(s, StrgHash.hashString s)
fun fromRlit (RLIT(s, _)) = s
fun rlitcmp (RLIT(s1,i1), RLIT(s2,i2)) = 
  if i1 < i2 then LESS
  else if i1 > i2 then GREATER else String.compare(s1, s2)
structure RlitDict = BinaryDict(struct type ord_key = rlit
                                       val cmpKey = rlitcmp
                                end)

(* lifting all literals from a CPS program *)
fun liftlits (body, root, offset) = 
  let (* the list of record, string, or real constants *)
      val m : info Intmap.intmap = Intmap.new(32, LitInfo)
      val freevars : lvar list ref = ref []
      fun addv x = (freevars := (x :: (!freevars)))

      (* check if an lvar is used by the main program *)
      val refset : Intset.intset = Intset.new()
      val used : lvar -> unit = Intset.add refset 
      val isUsed : lvar -> bool = Intset.mem refset

      (* memoize the information on which corresponds to what *)
      fun enter (v, i) = (Intmap.add m (v, i); addv v)
      fun const (VAR v) = ((Intmap.map m v; true) handle _ => false)
        | const (INT _ | INT32 _ | REAL _ | STRING _) = true
        | const _ = bug "unexpected case in const"

      (* register a string literal *)
      local val strs : string list ref = ref []
            val strsN : int ref = ref 0
            val sdict = ref (RlitDict.mkDict())
            val srtv = mkv()
            val srtval = VAR srtv
      in
      fun entStr s = 
        let val v = mkv()  (** should hash to remove duplicates **)
            val sd = !sdict
            val rlit = toRlit s
            val n = 
              (case RlitDict.peek(sd, rlit)
                of SOME k => k
                 | _ => let val _ = (strs := (s :: (!strs)))
                            val k = !strsN
                            val _ = (strsN := (k+1)) 
                            val _ = (sdict := (RlitDict.insert(sd, rlit, k)))
                         in k
                        end)
         in (VAR v, fn ce => SELECT(n, srtval, v, BOGt, ce))
        end

(* old definition of entStr
        
        let val sd = !sdict
            val rlit = toRlit s
         in (case RlitDict.peek(sd, rlit)
              of SOME v => (VAR v, ident)
               | _ => let val v = mkv()
                          val _ = (enter(v, ZZ_STR s); used v)
                          val _ = (sdict := RlitDict.insert(sd, rlit, v))
                       in (VAR v, ident)
                      end)
        end
*)

      fun appStr () = 
        let fun g (a::r, z) = g(r, (STRING a)::z)  
              | g ([], z) = z (* reverse to reflecting the correct order *)
            val allStrs = !strs
         in case !strs
             of [] => ()
              | xs => (enter(srtv, ZZ_RCD(RK_RECORD, g(xs,[]))); used srtv)
        end
      end (* local of processing string literals *)
      
      (** a special treatment of real constants *)
      local val reals : string list ref = ref []
            val realsN : int ref = ref 0
            val rdict = ref (RlitDict.mkDict())
            val rrtv = mkv()
            val rrtval = VAR rrtv
      in				       
      fun entReal s = 
        let val v = mkv()  (** should hash to remove duplicates **)
            val rd = !rdict
            val rlit = toRlit s
            val n = 
              (case RlitDict.peek(rd, rlit)
                of SOME k => k
                 | _ => let val _ = (reals := (s :: (!reals)))
                            val k = !realsN
                            val _ = (realsN := (k+1)) 
                            val _ = (rdict := (RlitDict.insert(rd, rlit, k)))
                         in k
                        end)
         in (VAR v, fn ce => SELECT(n, rrtval, v, FLTt, ce))
        end

      fun appReal () = 
        let fun g (a::r, z) = g(r, (REAL a)::z)  
              | g ([], z) = z (* reverse to reflecting the correct order *)
            val allReals = !reals
         in case !reals 
             of [] => ()
              | xs => (enter(rrtv, ZZ_RCD(RK_FBLOCK, g(xs,[]))); used rrtv)
        end
      end (* local of special treatment of real constants *)

      (* translation on the CPS values *)
      fun lpsv u = 
        (case u
          of REAL s => entReal s
           | STRING s => entStr s
           | VAR v => (used v; (u, ident))
           | _ => (u, ident))

      fun lpvs vs = 
        let fun g (u, (xs, hh)) = 
              let val (nu, nh) = lpsv u 
               in (nu::xs, nh o hh) 
              end
         in foldr g ([], ident) vs
        end

      (* if all fields of a record are "constant", then we lift it *)
      fun field ul = 
        let fun h ((x, OFFp 0)::r, z) = 
                 if const x then h(r, x::z) else NONE
              | h ([], z) = SOME(rev z)
              | h _ = bug "unexpected case in field"
         in h (ul, [])
        end

      (* register a constant record *)
      fun record (rk, ul, v) =
        (case field ul
          of SOME xl => (enter(v, ZZ_RCD(rk, xl)); ident)
           | NONE =>
               let fun g ((u, p as OFFp 0), (r, hh)) = 
                         let val (nu, nh) = lpsv u
                          in ((nu, p)::r, nh o hh)
                         end
                     | g _ = bug "unexpected non-zero OFFp in record"
                   val (nl, hdr) = foldr g ([], ident) ul
                in fn ce => hdr(RECORD(rk, nl, v, ce))
               end)

      (* register a wrapped float literal *)
      fun wrapfloat (u, v, t) = 
        if const u then (enter(v, ZZ_RCD(RK_FBLOCK, [u])); ident)
        else let val (nu, hh) = lpsv u
              in (fn ce => hh(PURE(P.fwrap, [nu], v, t, ce)))
             end

      (* fetch out the literal information *)
      fun getInfo () = 
        let val _ = appReal()   (* register all Reals as a record *)
            val _ = appStr()   (* register all Strings as a record *)
            val allvars = !freevars
            val exports = List.filter isUsed allvars

            val toplit = 
              let fun g ([], z) = LI_TOP z
                    | g (x::r, z) = 
                         (case Intmap.map m x
                           of ZZ_STR s => g(r, (LI_STRING s)::z)
                            | _ => g(r, (LI_VAR x)::z))
               in g(exports, [])
              end

            fun mklit (v, lit) = 
              (case Intmap.map m v
                of (ZZ_FLT _) => (* float is wrapped *)
                     bug "currently we don't expect ZZ_FLT in mklit"
                     (* LI_RECORD(RK_FBLOCK, [LI_REAL s], v, lit) *)
                 | (ZZ_STR s) => 
                     bug "currently we don't expect ZZ_STR in mklit"
                     (* lit   --- or we could inline string *)
                 | (ZZ_RCD (rk, vs)) => 
                     LI_RECORD(rk, map val2lit vs, v, lit))

            (** build up the literal structure *)
            val lit = foldl mklit toplit allvars

            val n = length exports
            val hdr = 
              if n = 0 then ident
              else let val rv = mkv()
                       val rval = VAR rv
                       val rhdr = 
                         fn ce => SELECT(offset, root, rv, PTRt(RPT n), ce)

                       fun mkhdr (v, (i, hh)) = 
                         let val nh = 
                               (case Intmap.map m v
                                 of (ZZ_FLT _) => bug "ZZ_FLT in mkhdr"
                                      (* (fn ce => 
                                           (SELECT(i, rval, w, PTRt(FPT 1),
                                            SELECT(0, VAR w, v, FLTt, ce)))) *)
                                  | (ZZ_STR s) => bug "ZZ_STR in mkhdr"
                                      (* (fn ce => 
                                            SELECT(i, rval, v, BOGt, ce)) *)
                                  | (ZZ_RCD (rk, vs)) =>
                                      let val n = length vs
                                          val t = 
                                            case rk 
                                             of RK_FBLOCK => PTRt(FPT n)
                                              | _ => PTRt(RPT n)
                                       in fn ce => SELECT(i, rval, v, t, ce)
                                      end)
                          in (i+1, hh o nh)
                         end
                    in #2 (foldr mkhdr (0, rhdr) exports)
                   end
         in (lit, hdr)
        end (* function getInfo *)

      fun lpfn (fk, f, vl, cl, e) = (fk, f, vl, cl, loop e)

      and loop ce =
        (case ce
          of RECORD (rk, ul, v, e) => record (rk, ul, v) (loop e)
           | SELECT (i, u, v, t, e) => 
               let val (nu, hh) = lpsv u
                in hh(SELECT(i, nu, v, t, loop e))
               end
           | OFFSET _ => bug "unexpected OFFSET in loop"
           | APP (u, ul) => 
               let val (nu, h1) = lpsv u
                   val (nl, h2) = lpvs ul
                in h1(h2(APP(nu, nl)))
               end
           | FIX (fns, e) => FIX(map lpfn fns, loop e)
           | SWITCH (u, v, es) => 
               let val (nu, hh) = lpsv u
                in hh(SWITCH(nu, v, map loop es))
               end
           | BRANCH (p, ul, v, e1, e2) => 
               let val (nl, hh) = lpvs ul
                in hh(BRANCH(p, nl, v, loop e1, loop e2))
               end
           | SETTER (p, ul, e) => 
               let val (nl, hh) = lpvs ul
                in hh(SETTER(p, nl, loop e))
               end
           | LOOKER (p, ul, v, t, e) =>
               let val (nl, hh) = lpvs ul
                in hh(LOOKER(p, nl, v, t, loop e))
               end
           | ARITH (p, ul, v, t, e) =>
               let val (nl, hh) = lpvs ul
                in hh(ARITH(p, nl, v, t, loop e))
               end
           | PURE (P.fwrap, [u], v, t, e) => wrapfloat (u, v, t) (loop e)
           | PURE (p, ul, v, t, e) => 
               let val (nl, hh) = lpvs ul
                in hh(PURE(p, nl, v, t, loop e))
               end)

      val newbody = loop body
      val (lit, hdr) = getInfo ()
   in (hdr newbody, lit)
  end

(* the main function *)
fun litsplit (fk, f, vl as [_,x], [CNTt, t as PTRt(RPT n)], body) = 
      let val nt = PTRt(RPT (n+1))
          val (nbody, lit) =  
            if !liftLiterals then liftlits(body, VAR x, n)
            else (body, LI_TOP [])
          
       in ((fk, f, vl, [CNTt, nt], nbody), lit)
      end
  | litsplit _ = bug "unexpected CPS header in litsplit"

end (* toplevel local *)
end (* Literals *)

