(* COPYRIGHT (c) 1998 YALE FLINT PROJECT *)
(* transtypes.sml *)

signature TRANSTYPES = 
sig
  val tpsKnd : Types.tycpath -> PLambdaType.tkind
  val tpsTyc : DebIndex.depth -> Types.tycpath -> PLambdaType.tyc

  val toTyc  : DebIndex.depth -> Types.ty -> PLambdaType.tyc
  val toLty  : DebIndex.depth -> Types.ty -> PLambdaType.lty
end (* signature TRANSTYPES *)

structure TransTypes : TRANSTYPES = 
struct
local structure BT = BasicTypes
      structure DI = DebIndex
      structure PT = PrimTyc
      structure LT = PLambdaType
      structure TU = TypesUtil
      open Types

      val tcAppSt = LT.tcc_app
in

fun bug msg = ErrorMsg.impossible ("TransTypes: " ^ msg)
val say = Control.Print.say 

local
structure PP = PrettyPrint
structure EM = ErrorMsg
in
val env = StaticEnv.empty
fun ppType x = 
 ((PP.with_pp (EM.defaultConsumer())
           (fn ppstrm => (PP.add_string ppstrm "find: ";
                          PPType.resetPPType();
                          PPType.ppType env ppstrm x)))
  handle _ => say "fail to print anything")

fun ppTycon x = 
 ((PP.with_pp (EM.defaultConsumer())
           (fn ppstrm => (PP.add_string ppstrm "find: ";
                          PPType.resetPPType();
                          PPType.ppTycon env ppstrm x)))
  handle _ => say "fail to print anything")
end

local val recTyContext = ref [~1]
in 
fun enterRecTy (a) = (recTyContext := (a::(!recTyContext)))
fun exitRecTy () = (recTyContext := tl (!recTyContext))
fun recTyc (i) = 
      let val x = hd(!recTyContext)
          val base = DI.innermost
       in if x = 0 then LT.tcc_var(base, i)
          else if x > 0 then LT.tcc_var(DI.di_inner base, i)
               else bug "unexpected RECtyc"
      end
fun freeTyc (i) = 
      let val x = hd(!recTyContext)
          val base = DI.di_inner (DI.innermost)
       in if x = 0 then LT.tcc_var(base, i)
          else if x > 0 then LT.tcc_var(DI.di_inner base, i)
               else bug "unexpected RECtyc"
      end
end (* end of recTyc and freeTyc hack *)

fun tpsKnd (TP_VAR {kind, ...}) = kind
  | tpsKnd _ = bug "unexpected tycpath parameters in tpsKnd"

fun tpsTyc d tp = 
  let fun h (TP_VAR {depth, num, ...}, cur) = 
              LT.tcc_var(DI.calc(cur, depth), num)
        | h (TP_TYC tc, cur) = tycTyc(tc, cur)
        | h (TP_SEL (tp, i), cur) = LT.tcc_proj(h(tp, cur), i)
        | h (TP_APP (tp, ps), cur) = 
              LT.tcc_app(h(tp, cur), map (fn x => h(x, cur)) ps)
        | h (TP_FCT (ps, ts), cur) = 
              let val ks = map tpsKnd ps
                  val cur' = DI.next cur
                  val ts' = map (fn x => h(x, cur')) ts
               in LT.tcc_fn(ks, LT.tcc_seq ts')
              end

   in h(tp, d)
  end

(*
and tycTyc x = 
  Stats.doPhase(Stats.makePhase "Compiler 043 1-tycTyc") tycTyc0 x
*)

and tycTyc(tc, d) = 
  let fun dtsTyc nd ({dcons: dconDesc list, arity=i, ...} : dtmember) = 
            let val nnd = if i=0 then nd else DI.next nd
                fun f ({domain=NONE, rep, name}, r) = (LT.tcc_unit)::r
                  | f ({domain=SOME t, rep, name}, r) = (toTyc nnd t)::r

                val _ = enterRecTy i
                val core = LT.tcc_sum(foldr f [] dcons)
                val _ = exitRecTy()

                val resTyc = if i=0 then core
                             else (let val ks = LT.tkc_arg i
                                    in LT.tcc_fn(ks, core)
                                   end)
             in (LT.tkc_int i, resTyc)
            end

      fun dtsFam (_, {lambdatyc=ref (SOME (tc,od)), ...} : dtypeFamily) =
            LT.tc_adj(tc, od, d) (* invariant: tc contains no free variables 
                                    so tc_adj should have no effects *)
        | dtsFam (freetycs, {members, lambdatyc=x, ...}) = 
            let fun ttk (GENtyc{arity=i, ...}) = LT.tkc_int i
                  | ttk (DEFtyc{tyfun=TYFUN{arity=i, ...},...}) = LT.tkc_int i
                  | ttk _ = bug "unexpected ttk in dtsFam"
                val ks = map ttk freetycs
                val (nd, hdr) = 
                  case ks of [] => (d, fn t => t)
                           | _ => (DI.next d, fn t => LT.tcc_fn(ks, t))
                val mbs = Vector.foldr (op ::) nil members
                val mtcs = map (dtsTyc (DI.next nd)) mbs
                val (fks, fts) = ListPair.unzip mtcs
                val nft = case fts of [x] => x | _ => LT.tcc_seq fts
                val tc = hdr(LT.tcc_fn(fks, nft)) 
                val _ = (x := SOME(tc, d))
             in tc
            end

      fun h (PRIMITIVE pt, _) = LT.tcc_prim(pt)
        | h (DATATYPE {index, family, freetycs, stamps, root}, _) = 
              let val tc = dtsFam (freetycs, family)
                  val n = Vector.length stamps 
                  (* invariant: n should be the length of family members *)
               in LT.tcc_fix((n, tc, (map g freetycs)), index)
              end
        | h (ABSTRACT tc, 0) = (g tc) 
              (*>>> LT.tcc_abs(g tc) <<<*) 
        | h (ABSTRACT tc, n) = (g tc) 
              (*>>> we tempoarily turned off the use of abstract tycons in
                    the intermediate language; proper support of ML-like
                    abstract types in the IL may require changes to the
                    ML language. (ZHONG)
              let val ks = LT.tkc_arg n
                  fun fromto(i,j) = if i < j then (i::fromto(i+1,j)) else []
                  val fs = fromto(0, n)
                  val ts = map (fn i => LT.tcc_var(DI.innermost, i)) fs
                  val b = tcAppSt(tycTyc(tc, DI.next d), ts)
               in LT.tcc_fn(ks, LT.tcc_abs b)
              end
              <<<*)
        | h (FLEXTYC tp, _) = tpsTyc d tp
        | h (FORMAL, _) = bug "unexpected FORMAL kind in tycTyc-h"
        | h (TEMP, _) = bug "unexpected TEMP kind in tycTyc-h"

      and g (tycon as (GENtyc{kind as DATATYPE _, arity, ...})) = 
              if TU.eqTycon(tycon, BT.refTycon) then LT.tcc_prim (PT.ptc_ref)
              else h(kind, arity)
        | g (GENtyc{kind, arity, ...}) = h(kind, arity)
        | g (DEFtyc{tyfun, ...}) = tfTyc(tyfun, d)
        | g (RECtyc i) = recTyc i
        | g (FREEtyc i) = freeTyc i
        | g (RECORDtyc _) = bug "unexpected RECORDtyc in tycTyc-g"
        | g (PATHtyc{arity, path=InvPath.IPATH ss, entPath}) = 
              ((* say "*** Warning for compiler writers: PATHtyc ";
               app (fn x => (say (Symbol.name x); say ".")) ss;
               say " in translate: ";
               say (EntPath.entPathToString entPath);
               say "\n"; *)
               if arity > 0 then LT.tcc_fn(LT.tkc_arg arity, LT.tcc_void)
               else LT.tcc_void)
        | g (ERRORtyc) = bug "unexpected tycon in tycTyc-g"

   in (g tc) 
  end

and tfTyc (TYFUN{arity=0, body}, d) = toTyc d body
  | tfTyc (TYFUN{arity, body}, d) = 
      let val ks = LT.tkc_arg arity
       in LT.tcc_fn(ks, toTyc (DI.next d) body)
      end

and toTyc d t = 
  let val m : (tyvar * LT.tyc) list ref = ref []
      fun lookTv tv = 
        let val xxx = !m
            fun uu ((a,x)::r) = if a = tv then x else uu r
              | uu [] = let val zz = h (!tv)
                            val _ = (m := ((tv,zz)::xxx))
                         in zz
                        end
         in uu xxx
        end

      and h (INSTANTIATED t) = g t
        | h (LBOUND {depth, num}) = LT.tcc_var(DI.calc(d, depth), num)
        | h (OPEN _) = LT.tcc_void
        | h _ = LT.tcc_void  (* ZHONG? *)

      and g (VARty tv) = (* h(!tv) *) lookTv tv
        | g (CONty(RECORDtyc _, [])) = LT.tcc_unit
        | g (CONty(RECORDtyc _, ts)) = LT.tcc_tuple (map g ts)
        | g (CONty(tyc, [])) = tycTyc(tyc, d)
        | g (CONty(DEFtyc{tyfun,...}, args)) = g(TU.applyTyfun(tyfun,args))
        | g (CONty(tc as GENtyc {kind=ABSTRACT _, ...}, ts)) = 
              tcAppSt(tycTyc(tc, d), map g ts)
        | g (CONty(tc as GENtyc _, [t1, t2])) = 
              if TU.eqTycon(tc, BT.arrowTycon) then LT.tcc_parrow(g t1, g t2)
              else LT.tcc_app(tycTyc(tc, d), [g t1, g t2])
        | g (CONty(tyc, ts)) = LT.tcc_app(tycTyc(tyc, d), map g ts)
        | g (IBOUND i) = LT.tcc_var(DI.innermost, i)
        | g (POLYty _) = bug "unexpected poly-type in toTyc"
        | g (UNDEFty) = bug "unexpected undef-type in toTyc"
        | g (WILDCARDty) = bug "unexpected wildcard-type in toTyc"

   in (g t) 
  end

and toLty d (POLYty {tyfun=TYFUN{arity=0, body}, ...}) = toLty d body
  | toLty d (POLYty {tyfun=TYFUN{arity, body},...}) = 
      let val ks = LT.tkc_arg arity
       in LT.ltc_poly(ks, [toLty (DI.next d) body])
      end

  | toLty d x = LT.ltc_tyc (toTyc d x)

(*
val toTyc  = 
  (fn x => fn y => 
     (Stats.doPhase(Stats.makePhase "Compiler 043 2-toTyc") (toTyc x) y))

val toLty  = 
  (fn x => fn y => 
     (Stats.doPhase(Stats.makePhase "Compiler 043 3-toLty") (toLty x) y))
*)

end (* toplevel local *)
end (* structure TransTypes *)


