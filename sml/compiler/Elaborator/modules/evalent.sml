(* Copyright 1996 by AT&T Bell Laboratories *)
(* evalent.sml *)

signature EVALENTITY =
sig 

  val evalApp : Modules.fctEntity * Modules.strEntity 
                * EntPathContext.context
                * InvPath.path * ElabUtil.compInfo
                -> Modules.strEntity 

  val debugging : bool ref

end (* signature EVALENTITY *)

structure EvalEntity : EVALENTITY =
struct

local 
      structure EP = EntPath
      structure IP = InvPath
      structure S = SourceMap
      structure T = Types
      structure EE = EntityEnv
      structure EPC = EntPathContext
      structure EU = ElabUtil
      structure MI = ModuleId
      structure MU = ModuleUtil
      open Modules 
in

(* debugging *)
val say = Control_Print.say
val debugging = (* ElabDataControl.eedebugging *) ref true
fun debugmsg (msg: string) =
    if !debugging then (say msg; say "\n") else ()

open ElabDebug

val debugPrint = (fn x => debugPrint debugging x)  (* Value Restriction *)
fun bug msg = ErrorMsg.impossible ("EvalEntity: " ^ msg);

(* special symbols -- defined in SpecialSymbols *)
val resultId = SpecialSymbols.resultId
val returnId = SpecialSymbols.returnId
val anonFctSym = SpecialSymbols.anonFctSym
val paramSym = SpecialSymbols.paramSym

val defaultError =
    ErrorMsg.errorNoFile(ErrorMsg.defaultConsumer(),ref false) (0,0)

(* local "conditional" variant of EntPathContext.enterOpen *)
fun enterOpen(epc: EPC.context, NONE: EntPath.entVar option) = epc
  | enterOpen(epc, SOME entv) = EPC.enterOpen(epc,entv)

fun evalTyc (entv, tycExp, entEnv, epc, rpath, 
             compInfo as {mkStamp,...}: EU.compInfo) : T.tycon =
      case tycExp
       of CONSTtyc tycon => tycon
        | FORMtyc (T.GENtyc { kind, arity, eq, path, ... }) =>
	  (case kind of
	       T.DATATYPE{index=0, stamps, freetycs, family, root=NONE} =>
               let val viztyc = MU.transTycon entEnv
                   val nstamps = Vector.map (fn _ => mkStamp()) stamps
                   val nst = Vector.sub(nstamps,0)
                   val nfreetycs = map viztyc freetycs
                   val _ = EPC.bindTycEntVar (epc, nst, entv)
               in
		   T.GENtyc{stamp=nst, arity=arity, eq=eq,
                            kind=T.DATATYPE{index=0, stamps=nstamps,
					    root=NONE,
					    freetycs=nfreetycs,
					    family=family},
                            path=IP.append(rpath,path), stub=NONE}
               end
             | T.DATATYPE{index=i, root=SOME rtev, ...} =>
               let val (nstamps, nfreetycs, nfamily) = 
                       case EE.lookTycEnt(entEnv, rtev)
			of T.GENtyc { kind = T.DATATYPE dt, ... } =>
			   (#stamps dt, #freetycs dt, #family dt)
			 | _ => bug "unexpected case in evalTyc-FMGENtyc (2)"
                   val nst = Vector.sub(nstamps,i)
                   val _ = EPC.bindTycEntVar (epc, nst, entv)
               in
		   T.GENtyc{stamp=nst, arity=arity,
                            kind=T.DATATYPE{index=i, stamps=nstamps,
					    root=NONE,
					    freetycs=nfreetycs,
					    family=nfamily},
                            path=IP.append(rpath,path),
			    eq=eq, stub=NONE}
               end
	     | _ => bug "unexpected GENtyc in evalTyc")
        | FORMtyc (T.DEFtyc{stamp,tyfun=T.TYFUN{arity, body},strict,path}) =>
          let val nst = mkStamp()
	      (* tycId=stamp (this should perhaps be more abstract some day) *)
	      val _ = EPC.bindTycEntVar (epc, nst, entv)
	   in T.DEFtyc{stamp=nst,
		       tyfun=T.TYFUN{arity=arity, 
 				     body=MU.transType entEnv body}, 
		       strict=strict, path=IP.append(rpath,path)}
          end
        | VARtyc entPath => 
	    (debugmsg (">>evalTyc[VARtyc]: "^EP.entPathToString entPath);
	     EE.lookTycEP(entEnv,entPath))
        | _ => bug "unexpected tycExp in evalTyc"

and evalStr(strExp, epc, entsvOp, entEnv, rpath, 
            compInfo as {mkStamp,...}: EU.compInfo)
         : strEntity * entityEnv =
     (debugmsg ("[Inside EvalStr ......");
      case strExp
       of VARstr entPath =>
	    (debugmsg (">>evalStr[VARstr]: "^EP.entPathToString entPath);
	     (EE.lookStrEP(entEnv,entPath), entEnv))

        | CONSTstr strEnt => (debugmsg ("--evalStr[CONSTstr]"); (strEnt, entEnv))

        | STRUCTURE {stamp, entDec} =>
            let val _ = debugmsg "--evalStr[STRUCTURE]"
		val epc = enterOpen(epc, entsvOp)
                val stp = evalStamp(stamp, epc, entEnv, compInfo) 
                val env = evalDec(entDec, epc, entEnv, rpath, compInfo)
	    in
		({stamp = stp, entities=env,
		  rpath = rpath, stub = NONE,
		  properties = PropList.newHolder ()},
		 entEnv)
            end

        | APPLY (fctExp, strExp) =>
	    let val _ = debugmsg "--evalStr[APPLY]"
		val (fctRlzn, entEnv1) = 
                    evalFct(fctExp, epc, entEnv, compInfo)
	        val (argRlzn, entEnv2) = 
                    evalStr(strExp, epc, entsvOp, entEnv1, 
                            IP.empty, compInfo)
		(* [GK Debug Printout] *)
		val _ = debugPrint ("--evalStr[APPLY]:fctRlzn=",
				    fn ppstrm => fn rlzn =>
			PPModules.ppEntity ppstrm (rlzn,StaticEnv.empty,100),
				    FCTent fctRlzn)
		val _ = debugPrint ("--evalStr[APPLY]:argRlzn=", 
				    fn ppstrm => fn rlzn =>
			PPModules.ppEntity ppstrm (rlzn,StaticEnv.empty,100),
				    STRent argRlzn)
                val epc = enterOpen(epc, entsvOp)
             in (evalApp(fctRlzn, argRlzn, epc, rpath, compInfo),
                 entEnv2)
            end

        | LETstr (entDec, strExp) =>
            let val entEnv1 = evalDec(entDec, epc,
                                      entEnv, rpath, compInfo)
                val (strEnt, entEnv2) = 
                    evalStr(strExp, epc, entsvOp, entEnv1, 
                            rpath, compInfo)

 	     in (strEnt, entEnv2)
            end

        | ABSstr (sign, strExp) => 
	    let val _ = debugmsg "--evalStr[ABSstr]"
		val (srcRlzn, entEnv1) =
                    evalStr(strExp,  epc, entsvOp, entEnv, rpath, compInfo)
                val {rlzn, primaryTycs} = 
                    Instantiate.instAbstr{sign=sign, entEnv=entEnv, 
					  srcRlzn=srcRlzn,
					  rpath=rpath, region=S.nullRegion,
					  compInfo=compInfo}

                (* because the abstraction creates a bunch of new stamps,
                   we have to bind them to the epcontext.
		   But not all new stamps are represented in abstycs, only
	           FORMALs (primaries)!
                 *)
                val epc = enterOpen(epc, entsvOp)
                fun bindEp (T.GENtyc gt, ep) =
		    EPC.bindTycEntPath (epc, MI.tycId gt, ep)
                  | bindEp _ = ()
                val _ = ListPair.app bindEp (abstycs, tyceps)
	     in (rlzn, entEnv1)
	    end

        | CONSTRAINstr {boundvar,raw,coercion} =>
            (* propagage the context rpath into the raw uncoerced structure *)
            let val (rawEnt, entEnv1) = 
                    evalStr(raw,  epc, SOME boundvar,
                            entEnv, rpath, compInfo)
                val entEnv2 = EE.bind(boundvar, STRent rawEnt, entEnv1)
                val (strEnt, entEnv3) = 
 	            evalStr(coercion,  epc, entsvOp, 
                            entEnv2, IP.empty, compInfo)
                
             in (strEnt, entEnv3)
            end

        | FORMstr _ => bug "unexpected FORMstr in evalStr")


and evalFct (fctExp,  epc, entEnv, 
             compInfo as {mkStamp,...}: EU.compInfo) =
    (debugmsg "--evalFct"; 
      case fctExp
       of VARfct entPath =>
	    (debugmsg (">>evalFct[VARfct]: "^EP.entPathToString entPath);
	     (EE.lookFctEP(entEnv,entPath), entEnv))

        | CONSTfct fctEntity => (fctEntity, entEnv)

        | LAMBDA{param, body, primaries, paramRlzn} => 
            let val _ = debugmsg "--evalFct[LAMBDA]"
	     in ({stamp = mkStamp (),
		  exp=fctExp,
		  closureEnv=entEnv,
		  rpath=IP.IPATH[anonFctSym],
		  stub=NONE,
		  properties = PropList.newHolder ()},
		 entEnv)
            end

        | LETfct (entDec, fctExp) =>
            let val entEnv1 = evalDec(entDec,  epc,
                                      entEnv, IP.empty, compInfo)
                val (fctEnt, entEnv2) = 
                  evalFct(fctExp,  epc, entEnv1, compInfo) 
             in (fctEnt, entEnv2)
            end)

and evalApp(fctRlzn : Modules.fctEntity, argRlzn, epc, rpath,
            compInfo as {mkStamp, ...} : EU.compInfo) = 
      let val {closureEnv=env,exp=LAMBDA{param, body, ...}, ...} = fctRlzn
	  val nenv = EE.mark(mkStamp, EE.bind(param, STRent argRlzn, env))
          val  _ = debugmsg ("[Inside EvalAPP] ......")
       in case body
           of (FORMstr(FSIG{paramsig, bodysig, ...})) => 
               let (** failing to add the stamps into the epcontext is
                       a potential bug here. Will fix this in the
                       future.  ZHONG. -- ??? doesn't bindEp below 
		       do this? DBM **)
		   val _ = debugmsg "--evalApp[FORMstr]"
                   val {rlzn, abstycs, tyceps} = 
                       Instantiate.instFormal {sign=bodysig, entEnv=nenv,
					       rpath=rpath, region=S.nullRegion,
					       compInfo=compInfo}

                   fun bindEp (T.GENtyc gt, ep) = 
                       EPC.bindTycEntPath (epc, MI.tycId gt, ep)
                     | bindEp _ = ()
                   val _ = ListPair.app bindEp (abstycs, tyceps)
                in rlzn
               end
            | _ => 
               let val _ = debugmsg "--evalApp[_]"
		   val (strEnt, deltaEE) =
                       evalStr(body,  epc, NONE, nenv, rpath, compInfo)
		       (* invariant: deltaEE should always be same as nenv
			  if the body of an functor is always a BaseStr. Notice 
			  functor body is constructed either in the source 
			  programs (ml.grm) or in the elabmod.sml when dealing 
			  with curried functor applications.
			*)
                in strEnt
               end
      end

and evalDec(dec, epc, entEnv, rpath,
            compInfo as {mkStamp,...}: EU.compInfo): entityEnv =
     (debugmsg ("[Inside EvalDec ......");
      case dec
       of TYCdec (entVar, tycExp) => 
            let val tycEnt = 
                    evalTyc(entVar, tycExp, entEnv, epc, rpath, compInfo)
	     in EE.bind(entVar, TYCent tycEnt, entEnv)
            end

        | STRdec (entVar, strExp, sym) => 
            let val rpath' = 
                    (* don't include returnId or resultId in rpaths *)
		    if Symbol.eq(sym, returnId)
		       orelse Symbol.eq(sym, resultId)
		    then rpath
		    else IP.extend(rpath,sym)
		val (strEnt, entEnv1) =
                    evalStr(strExp,  epc, SOME entVar,
                            entEnv, rpath', compInfo)
             in EE.bind(entVar, STRent strEnt, entEnv1)
            end

        | FCTdec (entVar, fctExp) => 
            let val _ = debugmsg "--evalDec[FCTdec]"
		val (fctEnt, entEnv1) = 
                    evalFct(fctExp, epc, entEnv, compInfo)
             in EE.bind(entVar, FCTent fctEnt, entEnv1)
            end          

        | SEQdec decs =>
            let fun h (dec, entEnv0) = 
                    evalDec(dec, epc, entEnv0, rpath, compInfo)
             in EE.mark(mkStamp, foldl h entEnv decs)
            end
        (* 
         * The following may be wrong, but since ASSERTION! the bound symbols 
         * are all distinct,it would not appear to cause any harm.
         *)
        | LOCALdec (localDec, bodyDec) => 
            let val entEnv1 = evalDec(localDec, epc,
                                      entEnv, IP.empty, compInfo)
             in evalDec(bodyDec, epc, entEnv1, rpath, compInfo)
            end

        | _  => entEnv)

(* evalStamp: evaluate a stamp expression. *)
and evalStamp (stpExp, epc, entEnv, 
               compInfo as {mkStamp,...}: EU.compInfo): Stamps.stamp =
      case stpExp
       of NEW => mkStamp()  (* generate a fresh stamp *)
        | GETSTAMP strExp => #stamp (#1 (evalStr(strExp, epc, NONE,
						 entEnv, IP.empty, compInfo)))
             (* evaluate a structure expression, then extract the stamp of the
	      * structure, throwing away the structure and the environment returned
	      * when it was evaluated *)
(*
val evalApp = Stats.doPhase(Stats.makePhase "Compiler 044 x-evalApp") evalApp
*)

end (* toplevel local *)
end (* structure EvalEntity *)
