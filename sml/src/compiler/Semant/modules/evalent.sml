(* Copyright 1996 by AT&T Bell Laboratories *)
(* evalent.sml *)

signature EVALENTITY =
sig 

  val evalApp : Modules.fctEntity * Modules.strEntity 
                * DebIndex.depth * EntPathContext.context
                * InvPath.path * ElabUtil.compInfo
                -> Modules.strEntity 

  val debugging : bool ref

end (* signature EVALENTITY *)

structure EvalEntity : EVALENTITY =
struct

local structure DI = DebIndex
      structure EP = EntPath
      structure IP = InvPath
      structure S = SourceMap
      structure T = Types
      structure EE = EntityEnv
      structure EPC = EntPathContext
      structure EU = ElabUtil
      structure I = Instantiate
      structure MI = ModuleId
      structure MU = ModuleUtil
      open Modules 
in 

(* debugging *)
val say = Control.Print.say
val debugging = Control.CG.eedebugging
fun debugmsg (msg: string) =
    if !debugging then (say msg; say "\n") else ()

open ElabDebug

val debugPrint = (fn x => debugPrint debugging x)  (* Value Restriction *)
fun bug msg = ErrorMsg.impossible ("EvalEntity: " ^ msg);
val anonFctSym = Symbol.fctSymbol "AnonFct"
val paramSym = Symbol.strSymbol "<FsigParamInst>"
val anonStrSym = Symbol.strSymbol "<AnonStr>"
val resultId = Symbol.strSymbol "<resultStr>"
val returnId = Symbol.strSymbol "<returnStr>"

val defaultError =
    ErrorMsg.errorNoFile(ErrorMsg.defaultConsumer(),ref false) (0,0)

fun evalTyc (entv, tycExp, entEnv, epc, rpath, 
             compInfo as {mkStamp,...}: EU.compInfo) =
      case tycExp
       of CONSTtyc tycon => tycon
        | FORMtyc (T.GENtyc {kind=T.DATATYPE{index=0, stamps, freetycs,
                                              family, root=NONE},
                              arity, eq, path, ...}) =>
            let val viztyc = MU.transTycon entEnv
                val nstamps = Vector.map (fn _ => mkStamp()) stamps
                val nst = Vector.sub(nstamps,0)
                val nfreetycs = map viztyc freetycs
                val _ = EPC.bindPath(epc,MI.TYCid(nst),entv)

             in T.GENtyc{stamp=nst, arity=arity, eq=eq,
                         kind=T.DATATYPE{index=0, stamps=nstamps, root=NONE,
                                         freetycs=nfreetycs, family=family},
                         path=IP.append(rpath,path)}
            end
        | FORMtyc (T.GENtyc {kind=T.DATATYPE{index=i, root=SOME rtev, ...},
                             arity, eq, path, ...}) => 
            let val (nstamps, nfreetycs, nfamily) = 
                  case EE.lookTycEnt(entEnv, rtev)
                   of (T.GENtyc{kind=T.DATATYPE{stamps,freetycs,family,...},
                                ...}) =>
                        (stamps, freetycs, family)
                    | _ => bug "unexpected case in evalTyc-FMGENtyc"
                val nst = Vector.sub(nstamps,i)
                val _ = EPC.bindPath(epc,MI.TYCid(nst),entv)

             in T.GENtyc{stamp=nst, arity=arity,
                         kind=T.DATATYPE{index=i, stamps=nstamps, root=NONE,
                                         freetycs=nfreetycs, family=nfamily},
                         path=IP.append(rpath,path), eq=eq} 
            end
        | FORMtyc (T.DEFtyc{stamp,tyfun=T.TYFUN{arity, body},strict,path}) =>
            let val nst = mkStamp()
                val _ = EPC.bindPath(epc,MI.TYCid(nst),entv)
             in T.DEFtyc{stamp = nst,
		         tyfun=T.TYFUN{arity=arity, 
 				       body=MU.transType entEnv body}, 
		         strict=strict, path=IP.append(rpath,path)}
            end
        | VARtyc entPath => 
	    (debugmsg (">>evalTyc[VARtyc]: "^EP.entPathToString entPath);
	     EE.lookTycEP(entEnv,entPath))
        | _ => bug "unexpected tycExp in evalTyc"

and evalStr(strExp, depth, epc, entsv, entEnv, rpath, 
            compInfo as {mkStamp,...}: EU.compInfo) =
     (debugmsg ("[Inside EvalStr ......");
      case strExp
       of VARstr entPath =>
	    (debugmsg (">>evalStr[VARstr]: "^EP.entPathToString entPath);
	     (EE.lookStrEP(entEnv,entPath), entEnv))

        | CONSTstr strEnt => (strEnt, entEnv)

        | STRUCTURE {stamp, entDec} =>
            let val epc = EPC.enterOpen(epc, entsv)
                val stp = evalStp(stamp, depth, epc, entEnv, compInfo) 
                val env = evalDec(entDec, depth, epc, entEnv, rpath, compInfo)
	     in ({stamp = stp, entities = env,
    	          lambdaty = ref NONE, rpath = rpath}, entEnv)
            end

        | APPLY (fctExp, strExp) =>
	    let val (fctRlzn, entEnv1) = 
                  evalFct(fctExp, depth, epc, entEnv, compInfo)
	        val (argRlzn, entEnv2) = 
                  evalStr(strExp, depth, epc, entsv, entEnv1, 
                          IP.empty, compInfo)
                val epc = EPC.enterOpen(epc, entsv)
             in (evalApp(fctRlzn, argRlzn, depth, epc, rpath, compInfo),
                 entEnv2)
            end

        | LETstr (entDec, strExp) =>
            let val entEnv1 = evalDec(entDec, depth, epc,
                                      entEnv, rpath, compInfo)
                val (strEnt, entEnv2) = 
                  evalStr(strExp, depth, epc, entsv, entEnv1, 
                          rpath, compInfo)

 	     in (strEnt, entEnv2)
            end

        | ABSstr (sign, strExp) => 
	    let val (srcRlzn, entEnv1) = 
                  evalStr(strExp, depth, epc, entsv, entEnv, rpath, compInfo)
                val {rlzn=rlzn, abstycs=abstycs, tyceps=tyceps} = 
                  I.instAbstr{sign=sign, entEnv=entEnv, srcRlzn=srcRlzn,
                              rpath=rpath, 
                              region=S.nullRegion, compInfo=compInfo}

                (* because the abstraction creates a bunch of new stamps,
                   we have to bind them to the epcontext.
                 *)
                val epc = EPC.enterOpen(epc, entsv)
                fun h (T.GENtyc{stamp, ...}, ep) = 
                         EPC.bindLongPath(epc,MI.TYCid(stamp),ep)
                  | h _ = ()
                val _ = ListPair.app h (abstycs, tyceps)
	     in (rlzn, entEnv1)
	    end

        | CONSTRAINstr {boundvar,raw,coercion} =>
            (* propagage the context rpath into the raw uncoerced structure *)
            let val (rawEnt, entEnv1) = 
                  evalStr(raw, depth, epc, SOME boundvar,
                          entEnv, rpath, compInfo)
                val entEnv2 = EE.bind(boundvar, STRent rawEnt, entEnv1)
            (*  val entEnv' = EE.bind(boundvar, STRent rawEnt, entEnv) *)
                val (strEnt, entEnv3) = 
 	          evalStr(coercion, depth, epc, entsv, 
                          entEnv2, IP.empty, compInfo)
                
             in (strEnt, entEnv3)
            end

        | FORMstr _ => bug "unexpected FORMstr in evalStr")


and evalFct (fctExp, depth, epc, entEnv, 
             compInfo as {mkStamp,...}: EU.compInfo) =
      case fctExp
       of VARfct entPath =>
	    (debugmsg (">>evalFct[VARfct]: "^EP.entPathToString entPath);
	     (EE.lookFctEP(entEnv,entPath), entEnv))

        | CONSTfct fctEntity => (fctEntity, entEnv)

        | LAMBDA{param, body} => 
            let val clos = CLOSURE{param=param, body=body, env=entEnv}
	     in ({stamp=mkStamp(), closure=clos, lambdaty=ref NONE,
  	          tycpath=NONE, rpath=IP.IPATH[anonFctSym]}, entEnv)
            end

        | LAMBDA_TP{param, body, sign as FSIG{paramsig, bodysig, ...}} =>
            let val clos = CLOSURE{param=param, body=body, env=entEnv} 
                val tps = 
                  let val rpath' = IP.IPATH [paramSym]
                      val {rlzn=paramEnt, tycpaths=paramTps} =
                        I.instParam{sign=paramsig, entEnv=entEnv, 
                                    rpath=rpath', depth=depth,
                                    region=S.nullRegion, compInfo=compInfo}
                      val entEnv' = 
                        EE.mark(mkStamp, EE.bind(param, STRent paramEnt, 
                                                 entEnv))
                      val (bodyRlzn,_) = 
                        evalStr(body, DI.next depth, epc, NONE,
                                entEnv', IP.empty, compInfo)
                      val bodyTps = 
                        I.getTycPaths{sign=bodysig, rlzn=bodyRlzn, 
                                      entEnv=entEnv', compInfo=compInfo}
                   in T.TP_FCT(paramTps, bodyTps)
                  end

             in ({stamp=mkStamp(), closure=clos, lambdaty=ref NONE,
                 tycpath=SOME tps, rpath=IP.IPATH[anonFctSym]}, entEnv)
            end

        | LETfct (entDec, fctExp) =>
            let val entEnv1 = evalDec(entDec, depth, epc,
                                      entEnv, IP.empty, compInfo)
                val (fctEnt, entEnv2) = 
                  evalFct(fctExp, depth, epc, entEnv1, compInfo) 
             in (fctEnt, entEnv2)
            end

        | _ => bug "unexpected cases in evalFct"

and evalApp(fctRlzn as {closure=CLOSURE{param, body, env}, tycpath, ...} : 
            Modules.fctEntity, argRlzn, depth, epc, rpath,
            compInfo as {mkStamp, ...} : EU.compInfo) = 
      let val nenv = EE.mark(mkStamp, EE.bind(param, STRent argRlzn, env))
          val  _ = debugmsg ("[Inside EvalAPP] ......")
       in case (body, tycpath)
           of (FORMstr(FSIG{paramsig, bodysig, ...}), SOME tp) => 
               let val argTps = I.getTycPaths{sign=paramsig, rlzn=argRlzn,
                                              entEnv=env, compInfo=compInfo}
                   val resTp = T.TP_APP(tp, argTps)

                   (** failing to add the stamps into the epcontext is
                       a potential bug here. Will fix this in the
                       future.  ZHONG **)

                   val {rlzn=rlzn, abstycs=abstycs, tyceps=tyceps} = 
                     I.instFmBody {sign=bodysig, entEnv=nenv, tycpath=resTp,
                                   rpath=rpath, region=S.nullRegion,
                                   compInfo=compInfo}

                   fun h (T.GENtyc{stamp, ...}, ep) = 
                           EPC.bindLongPath(epc,MI.TYCid(stamp),ep)
                     | h _ = ()
                   val _ = ListPair.app h (abstycs, tyceps)
                in rlzn
               end
            | _ => 
               let val (strEnt, deltaEE)
                     = evalStr(body, depth, epc, NONE, nenv, rpath, compInfo)
                   (* invariant: deltaEE should always be same as nenv
                      if the body of an functor is always a BaseStr. Notice 
                      functor body is constructed either in the source 
                      programs (ml.grm) or in the elabmod.sml when dealing 
                      with curried functor applications.
                    *)
                in strEnt
               end
      end

and evalDec(dec, depth, epc, entEnv, rpath, 
            compInfo as {mkStamp,...}: EU.compInfo) =
     (debugmsg ("[Inside EvalDec ......");
      case dec
       of TYCdec (entVar, tycExp) => 
            let val tycEnt = 
                  evalTyc(entVar, tycExp, entEnv, epc, rpath, compInfo)
	     in EE.bind(entVar, TYCent tycEnt, entEnv)
            end
        | STRdec (entVar, strExp, sym) => 
            let val rpath' = 
		    if Symbol.eq(sym, returnId)
		       orelse Symbol.eq(sym, resultId)
		    then rpath
		    else IP.extend(rpath,sym)
		val (strEnt, entEnv1) =
                  evalStr(strExp, depth, epc, SOME entVar,
                          entEnv, rpath', compInfo)
             in EE.bind(entVar, STRent strEnt, entEnv1)
            end

        | FCTdec (entVar, fctExp) => 
            let val (fctEnt, entEnv1) = 
                  evalFct(fctExp, depth, epc, entEnv, compInfo)
             in EE.bind(entVar, FCTent fctEnt, entEnv1)
            end          
        | SEQdec decs =>
            let fun h (dec, entEnv0) = 
                  evalDec(dec, depth, epc, entEnv0, rpath, compInfo)
             in EE.mark(mkStamp, foldl h entEnv decs)
            end
        (* 
         * The following may be wrong, but since ASSERTION! the bound symbols 
         * are all distinct,it would not appear to cause any harm.
         *)
        | LOCALdec (localDec, bodyDec) => 
            let val entEnv1 = evalDec(localDec, depth, epc,
                                      entEnv, IP.empty, compInfo)
             in evalDec(bodyDec, depth, epc, entEnv1, rpath, compInfo)
            end

        | _  => entEnv)

and evalStp (stpExp, depth, epc, entEnv, 
             compInfo as {mkStamp,...}: EU.compInfo) =
      case stpExp
       of CONST stamp     => stamp
        | NEW             => mkStamp()
        | GETSTAMP strExp => 
            let val (strEnt, _) = 
                  evalStr(strExp, depth, epc, NONE,
                          entEnv, IP.empty, compInfo)
             in #stamp(strEnt)
            end

(*
val evalApp = Stats.doPhase(Stats.makePhase "Compiler 044 x-evalApp") evalApp
*)

end (* toplevel local *)
end (* structure EvalEntity *)

