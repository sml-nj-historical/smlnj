(* COPYRIGHT (c) 1998 YALE FLINT PROJECT *)
(* transmodules.sml *)

signature TRANSMODULES = 
sig 

  (* 
   * Invariant: strLty and fctLty are called only inside the translate.sml.
   * Furthermore, they are called upon top-level structures and functors
   * or formal structures instantiated by Instantiate.instParam only.
   *)

  val strLty  : Modules.Structure * DebIndex.depth 
                    * ElabUtil.compInfo -> PLambdaType.lty
  val fctLty  : Modules.Functor * DebIndex.depth 
                    * ElabUtil.compInfo -> PLambdaType.lty

  val debugging : bool ref

end (* signature TRANSMODULES *)

structure TransModules : TRANSMODULES =
struct 

local structure DA = Access
      structure DI = DebIndex
      structure EE = EntityEnv
      structure EM = ErrorMsg
      structure IP = InvPath
      structure EPC = EntPathContext
      structure EV = EvalEntity
      structure INS = Instantiate
      structure LT = PLambdaType
      structure MU = ModuleUtil
      structure TP = Types
      structure TT = TransTypes
      structure SE = StaticEnv
      open Modules ElabDebug

in 

val debugging = Control.CG.tmdebugging
fun bug msg = EM.impossible("TransModule: "^msg)
val say = Control.Print.say

fun debugmsg (msg: string) =
  if !debugging then (say msg; say "\n") else ()

val debugPrint = (fn x => debugPrint debugging x)

val defaultError =
  EM.errorNoFile(EM.defaultConsumer(),ref false) SourceMap.nullRegion

fun specLty (elements, entEnv, depth, compInfo) = 
  let fun g ([], entEnv, ltys) = rev ltys
        | g ((sym, TYCspec _)::rest, entEnv, ltys) = g(rest, entEnv, ltys)
        | g ((sym, STRspec {sign, entVar, ...})::rest, entEnv, ltys) =
              let val rlzn = EE.lookStrEnt(entEnv,entVar)
                  val lt = strRlznLty(sign, rlzn, depth, compInfo) 
               in g(rest, entEnv, lt::ltys)
              end
        | g ((sym, FCTspec {sign, entVar, ...})::rest, entEnv, ltys) = 
              let val rlzn = EE.lookFctEnt(entEnv,entVar)
                  val lt = fctRlznLty(sign, rlzn, depth, compInfo) 
               in g(rest, entEnv, lt::ltys)
              end
        | g ((sym, spec)::rest, entEnv, ltys) =
              let val _ = debugmsg ">>specLtyElt"
                  fun transty ty = 
                    ((MU.transType entEnv ty)
                      handle EE.Unbound =>
                         (debugmsg "$specLty";
                          withInternals(fn () =>
                           debugPrint("entEnv: ",
                                 (fn pps => fn ee => 
                                  PPModules.ppEntityEnv pps (ee,SE.empty,12)),
                                  entEnv));
                          debugmsg ("$specLty: should have printed entEnv");
                          raise EE.Unbound))

                  fun mapty t = TT.toLty depth (transty t)

               in case spec
                   of VALspec{spec=typ,...} => 
                        g(rest, entEnv, (mapty typ)::ltys)
                    | CONspec{spec=TP.DATACON{rep=DA.EXN _, 
                                              typ, ...}, ...} => 
                        let val lt = mapty typ
                            val argt = 
                              if LT.ltp_parrow lt then #1(LT.ltd_parrow lt)
                              else LT.ltc_unit
                         in g(rest, entEnv, (LT.ltc_etag argt)::ltys)
                        end
                    | CONspec{spec=TP.DATACON _, ...} =>
                        g(rest, entEnv, ltys)
                    | _ => bug "unexpected spec in specLty"
              end

   in g (elements, entEnv, [])
  end

(*
and signLty (sign, depth, compInfo) = 
  let fun h (SIG {kind=SOME _, lambdaty=ref (SOME(lt, od)), ...}) = lt
             (* LT.lt_adj(lt, od, depth) *)
        | h (sign as SIG{kind=SOME _, lambdaty as ref NONE, ...}) = 
          (* Invariant: we assum that all Named signatures (kind=SOME _) are
           * defined at top-level, outside any functor definitions. (ZHONG)
           *)
             let val {rlzn=rlzn, tycpaths=tycpaths} = 
                   INS.instParam {sign=sign, entEnv=EE.empty, depth=depth,
                                  rpath=InvPath.IPATH[], compInfo=compInfo,
                                  region=SourceMap.nullRegion}
                 val nd = DI.next depth
                 val nlty = strMetaLty(sign, rlzn, nd, compInfo)

                 val ks = map TT.tpsKnd tycpaths
                 val lt = LT.ltc_poly(ks, nlty)
              in lambdaty := SOME (lt, depth); lt
             end
        | h _ = bug "unexpected sign in signLty"
   in h sign
  end
*)
and strMetaLty (sign, rlzn, depth, compInfo) = 
  let fun g (sign, rlzn as {lambdaty = ref (SOME (lt,od)), ...}) = 
             LT.lt_adj(lt, od, depth)
        | g (sign as SIG{elements, ...}, 
             rlzn as {entities, lambdaty, ...} : strEntity) = 
               let val ltys = specLty(elements, entities, depth, compInfo)
                   val lt = (* case ltys of [] => LT.ltc_int
                                       | _ => *) LT.ltc_str(ltys)
                in lambdaty := SOME(lt, depth); lt
               end
        | g _ = bug "unexpected sign and rlzn in strMetaLty"

   in g(sign, rlzn)
  end

and strRlznLty (sign, rlzn, depth, compInfo) = 
  let fun g (sign, rlzn as {lambdaty = ref (SOME (lt,od)), ...} : strEntity) = 
             LT.lt_adj(lt, od, depth)

(* Note: the code here is designed to improve the "toLty" translation;
   by translating the signature instead of the structure, this can 
   potentially save time on strLty. But it can increase the cost of
   other procedures. Thus we turn it off temporarily. (ZHONG)

        | g (sign as SIG{kind=SOME _, ...}, rlzn as {lambdaty, ...}) = 
             let val sgt = signLty(sign, depth, compInfo)
                 (* Invariant: we assum that all Named signatures 
                  * (kind=SOME _) are defined at top-level, outside any 
                  * functor definitions. (ZHONG)
                  *)
                 val argtycs = INS.getTycPaths{sign=sign, rlzn=rlzn,
                         entEnv=EE.empty, compInfo=compInfo}
                 val lt = LT.lt_inst(sgt, map (TT.tpsTyc depth) argtycs)
              in lambdaty := SOME(lt, depth); lt
             end
*)
        | g _ = strMetaLty(sign, rlzn, depth, compInfo)

   in g(sign, rlzn)
  end

and fctRlznLty (sign, rlzn, depth, compInfo) = 
  let fun g (sign, rlzn as {lambdaty = ref (SOME (lt, od)), ...}) = 
             LT.lt_adj(lt, od, depth)
        | g (sign as FSIG{paramsig, bodysig, ...},
             rlzn as {stamp, closure as CLOSURE{env,...}, lambdaty, ...}) = 
               let val {rlzn=argRlzn, tycpaths=tycpaths} = 
                     INS.instParam {sign=paramsig, entEnv=env, depth=depth, 
                                    rpath=InvPath.IPATH[], compInfo=compInfo,
                                    region=SourceMap.nullRegion}
                   val nd = DI.next depth
                   val paramLty = strMetaLty(paramsig, argRlzn, nd, compInfo)
                   val ks = map TT.tpsKnd tycpaths
                   val bodyRlzn = 
                     EV.evalApp(rlzn, argRlzn, nd, EPC.initContext,
                                IP.empty, compInfo)
                   val bodyLty = strRlznLty(bodysig, bodyRlzn, nd, compInfo)

                   val lt = LT.ltc_poly(ks, [LT.ltc_fct([paramLty], [bodyLty])])
                in lambdaty := SOME (lt, depth); lt
               end

        | g _ = bug "fctRlznLty"

   in g(sign, rlzn)
  end

and strLty (str, depth, compInfo) = 
  let fun g (STR{rlzn={lambdaty=ref (SOME (lt, od)), ...}, ...}) = 
              LT.lt_adj(lt, od, depth)
        | g (STR{sign, rlzn as {lambdaty as ref NONE, ...}, ...}) = 
              let val lt = strRlznLty(sign, rlzn, depth, compInfo)
               in (lambdaty := SOME(lt, depth); lt)
              end
        | g _ = bug "unexpected structure in strLty"
   in g str
  end

and fctLty (fct, depth, compInfo) = 
  let fun g (FCT{rlzn={lambdaty=ref(SOME (lt,od)),...}, ...}) = 
              LT.lt_adj(lt, od, depth)
        | g (FCT{sign, rlzn as {lambdaty as ref NONE, ...}, ...}) = 
              let val lt = fctRlznLty(sign, rlzn, depth, compInfo)
               in (lambdaty := SOME(lt,depth); lt)
              end
        | g _ = bug "unexpected functor in fctLty"
   in g fct
  end

(****************************************************************************
 *  Turn off all effects if !Control.CG.representations is false            * 
 ****************************************************************************)

val rep_flag = ref true (* Control.CG.representations <-- WRONG  *)
val bogusStr = LT.ltc_void
val bogusFct = LT.ltc_fct([bogusStr], [bogusStr])

val (strLty, fctLty) = 
  if !rep_flag then (strLty, fctLty) else (fn _ => bogusStr, fn _ => bogusFct)

(*
val strLty = Stats.doPhase(Stats.makePhase "Compiler 044 1-strLty") strLty
val fctLty = Stats.doPhase(Stats.makePhase "Compiler 044 2-fctLty") fctLty
*)
end (* top-level local *)
end (* structure TransModules *)


(*
 * $Log: transmodules.sml,v $
 * Revision 1.2  1997/04/02  04:16:53  dbm
 *   Added empty rpath parameter to call of EV.evalApp. (Fix for bug 12)
 *
 * Revision 1.1.1.1  1997/01/14  01:38:47  george
 *   Version 109.24
 *
 *)
