(* Copyright 1996 by AT&T Bell Laboratories *)
(* sigmatch.sml *)

signature SIGMATCH =
sig

  (*** these four functions are only called inside elabmod.sml ***)
  val matchStr : 
       {sign     : Modules.Signature, 
        str      : Modules.Structure,
        strExp   : Modules.strExp,
        entvar   : EntPath.entVar,
        entEnv   : Modules.entityEnv,
        rpath    : InvPath.path,
        statenv  : StaticEnv.staticEnv,
        region   : SourceMap.region,
        compInfo : ElabUtil.compInfo} -> {resDec : Absyn.dec,
                                          resStr : Modules.Structure,
                                          resExp : Modules.strExp}

  val matchFct : 
       {sign     : Modules.fctSig,
        fct      : Modules.Functor,
        fctExp   : Modules.fctExp,
        entEnv   : Modules.entityEnv,
        rpath    : InvPath.path,
        statenv  : StaticEnv.staticEnv,
        region   : SourceMap.region,
        compInfo : ElabUtil.compInfo} -> {resDec : Absyn.dec,
                                          resFct : Modules.Functor,
                                          resExp : Modules.fctExp}

  val packStr : 
       {sign     : Modules.Signature, 
        str      : Modules.Structure,
        strExp   : Modules.strExp,
        entEnv   : Modules.entityEnv,
        rpath    : InvPath.path,
        statenv  : StaticEnv.staticEnv,
        region   : SourceMap.region,
        compInfo : ElabUtil.compInfo} -> {resDec : Absyn.dec,
                                          resStr : Modules.Structure,
                                          resExp : Modules.strExp}

  val applyFct : 
       {fct      : Modules.Functor,
        fctExp   : Modules.fctExp,
        argStr   : Modules.Structure, 
        argExp   : Modules.strExp,
        entvar   : EntPath.entVar,
        epc      : EntPathContext.context,                                
        statenv  : StaticEnv.staticEnv,
	rpath    : InvPath.path,
        region   : SourceMap.region,
        compInfo : ElabUtil.compInfo} -> {resDec : Absyn.dec,
                                          resStr : Modules.Structure,
                                          resExp : Modules.strExp}


  val debugging : bool ref
  val showsigs : bool ref

end (* signature SIGMATCH *)


(* functorized to factor out dependencies on FLINT... *)
structure SigMatch : SIGMATCH =
struct

local structure EV = EvalEntity
      structure A  = Absyn
      structure B  = Bindings
      structure DA = Access
      structure EE = EntityEnv
      structure EM = ErrorMsg
      structure EP = EntPath
      structure EPC = EntPathContext
      structure EU = ElabUtil
      structure INS = Instantiate
      structure IP = InvPath
      structure M  = Modules
      structure MU = ModuleUtil
      structure PP = PrettyPrintNew
      structure PU = PPUtilNew
      structure S  = Symbol
      structure SE = StaticEnv
      structure SP = SymPath
      structure ST = Stamps
      structure T  = Types
      structure TU = TypesUtil
      structure V  = VarCon

      open Types Modules VarCon ElabDebug

in

exception BadBinding

val debugging = (* ElabControl.smdebugging *) ref true
val showsigs = ref false

val say = Control_Print.say
fun debugmsg (msg: string) =
      if !debugging then (say msg; say "\n") else ()

fun bug msg = EM.impossible ("SigMatch:" ^ msg)
val nth = List.nth
fun for l f = app f l

fun unTYCent (TYCent x) = x
  | unTYCent ERRORent = ERRORtyc (* [GK 5/7/07] Avoid secondary bug, bug 1599.1 *)
  | unTYCent _ = bug "unTYCent"

fun symbolsToString []  = ""
  | symbolsToString [n] = S.name n
  | symbolsToString (n::r) =
      concat(S.name n :: foldr (fn(n,b) => (","::S.name n::b)) [] r)

val BogusTy = UNDEFty 

(* 
 * Bogus coercion expressions returned by the matching functions.  These
 * should never be evaluated. 
 *)

val bogusStrExp = M.VARstr []
val bogusFctExp = M.VARfct []

fun showStr (msg,str) =
    withInternals(fn () =>
      debugPrint debugging
       (msg,
	(fn pps => fn str =>
	    PPModules.ppStructure pps (str, SE.empty, 100)),
	str))

fun exnRep (DA.EXN _, dacc) = DA.EXN dacc
  | exnRep _ = bug "unexpected conrep in exnRep"

fun isNamed(SOME _) = true
  | isNamed _ = false

val anonSym = S.strSymbol "<anonymousStr>"
val anonFsym = S.fctSymbol "<anonymousFct>"
val paramSym = S.strSymbol "<FsigParamInst>"

fun ident _ = ()

(* returns true and the new instantiations if actual type > spec type *)
(* matches an abstract version of a type with its actual version *)
(**
fun absEqvTy (spec, actual, dinfo) : (ty list * tyvar list * ty * bool) =
  let val actual = TU.prune actual
      val spec = TU.prune spec
      val (actinst, insttys0) = TU.instantiatePoly actual
      val (specinst, stys) = TU.instantiatePoly spec
      val _ = ListPair.app Unify.unifyTy (insttys0, stys)

      (*
       * This is a gross hack. Inlining-information such as primops 
       * (or inline-able expressions) are propagated through signature
       * matching. However, their types may change. The following code
       * is to figure out the proper type application arguments, insttys.
       * The typechecker does similar hack. We will clean this up in the
       * future (ZHONG).
       *
       * Change: The hack is gone, but I am not sure whether the code
       * below could be further simplified.  (INL_PRIM now has mandatory
       * type information, and this type information is always correctly
       * provided by prim.sml.)  (Blume, 1/2001)
       *)
(*
      val insttys = 
       (case dinfo 
         of II.INL_PRIM(_, st) =>
              (let val (actinst', insttys') = TU.instantiatePoly st
                in Unify.unifyTy(actinst', actinst) handle _ => ();
                   insttys'
               end)
          | _ =>insttys0)
*)
      val insttys = 
	  case InlInfo.primopTy dinfo of
	      SOME st =>
              (let val (actinst', insttys') = TU.instantiatePoly st
               in
		   Unify.unifyTy(actinst', actinst) handle _ => ();
		   insttys'
               end)
            | NONE =>insttys0

      val res = (Unify.unifyTy(actinst, specinst); true) handle _ => false
      (* dbm: shouldn't this unifyTy always succeed, because when called
       * in packElems, the structure will already have been matched against
       * the signature (according to the comment before packStr)  [KM ???]*)

      val instbtvs = map TU.tyvarType insttys0
      (* should I use stys here instead?, why insttys0? *)

   in (insttys, instbtvs, specinst, res)
  end
**)

(* dbm: obsolete!
fun eqvTnspTy (spec, actual, dinfo) : (ty list * tyvar list) = 
  let val actual = TU.prune actual
      val (actinst, insttys) = TU.instantiatePoly actual

      (*
       * This is a gross hack. Inlining-information such as primops 
       * (or inline-able expressions) are propagated through signature
       * matching. However, their types may change. The following code
       * is to figure out the proper type application arguments, insttys.
       * The typechecker does similar hack. We will clean this up in the
       * future (ZHONG).
       *
       * Change: The hack is gone, but I am not sure whether the code
       * below could be further simplified.  (INL_PRIM now has mandatory
       * type information, and this type information is always correctly
       * provided by prim.sml.)  (Blume, 1/2001)
       *)
(*
      val insttys = 
       (case dinfo 
         of II.INL_PRIM(_, st) =>
              (let val (actinst', insttys') = TU.instantiatePoly st
                in Unify.unifyTy(actinst', actinst) handle _ => ();
                   insttys'
               end)
          | _ =>insttys)
*)
      val insttys = 
	  case InlInfo.primopTy dinfo of
	      SOME st =>
              (let val (actinst', insttys') = TU.instantiatePoly st
               in
		   Unify.unifyTy(actinst', actinst) handle _ => ();
		   insttys'
               end)
            | NONE =>insttys

      val (specinst, stys) = TU.instantiatePoly spec
      val _ = ((Unify.unifyTy(actinst, specinst))
               handle _ => bug "unexpected types in eqvTnspTy")
      val btvs = map TU.tyvarType stys
             
   in (insttys, btvs)
  end
*)

(**************************************************************************
 *                                                                        *
 * Matching a structure against a signature:                              *
 *                                                                        *
 *  val matchStr1 : Signature * Structure * S.symbol *                    *
 *                  entityEnv * EP.entPath * IP.path * staticEnv *        *
 *                  region * EU.compInfo                                  *
 *                   -> A.dec * M.Structure * M.strExp                    *
 *                                                                        *
 * WARNING: epath is an inverse entPath, so it has to be reversed to      *
 *          produce an entPath.                                           *
 *                                                                        *
 **************************************************************************)
fun matchStr1(specSig as SIG{stamp=sigStamp,closed,fctflag,
			     elements=sigElements,...},
              str as STR { sign = SIG{stamp=strSigStamp,
				      elements=strElements,...},
			   rlzn as {stamp=strStamp,entities=strEntEnv,...},
			   access = rootAcc, prim = rootPrim },
              strName : S.symbol,
              matchEntEnv, 
              epath: EP.entVar list,
              rpath: IP.path,
              statenv, 
              region,
	      compInfo as {mkStamp, mkLvar=mkv, error, ...}: EU.compInfo) =
let

  val err = error region
  val _ = let fun h pps sign =PPModules.ppSignature pps (sign,statenv,6)
              val s = ">>matchStr1 - specSig :"
           in debugPrint (showsigs) (s, h, specSig)
          end

  (* matchTypes checks whether the spec type is a generic instance of
   * the actual type, and if so it returns two lists of type metavariables
   * (tyvars):
   *  (1) the spec type generic instantiation metavariables (btvs),
   *  (2) the actual type generic instantiation metavariables (ptvs).
   * In the matching, the btvs variables are not instantiated, while the
   * ptvs are always instantiated, and their instantiations constitute the
   * "parameters of instantiatiation" that make the actual type agree with
   * the (generic instance of the) spec. The types in the parameter
   * instantiations will contain occurrences of the bound tyvars.
   * If the actual is not a polytype, the ptvs list is nil. Similarly for
   * the spec type and btvs. If both spec and actual are monotypes, the
   * matching is equivalent to equalTypes(spec,actual). [dbm: 7/7/06]
   *)
  fun matchTypes (spec, actual, name) : (T.tyvar list * T.tyvar list) option =
      case TU.matchInstTypes(false, spec, actual)
       of x as SOME(btvs,ptvs) => x
        | NONE =>
          (err EM.COMPLAIN 
              "value type in structure doesn't match signature spec"
              (fn ppstrm =>
                   (PPType.resetPPType();
                    PP.newline ppstrm;
                    app (PP.string ppstrm) ["  name: ", S.name name];
                    PP.newline ppstrm;
                    PP.string ppstrm "spec:   ";
                    PPType.ppType statenv ppstrm spec;
                    PP.newline ppstrm;
                    PP.string ppstrm "actual: ";
                    PPType.ppType statenv ppstrm actual));
           NONE)

  fun complain s = err EM.COMPLAIN s EM.nullErrorBody
  fun complain' x = (complain x; raise BadBinding)

  (* 
   * Compare datacon names of spec and actual datatype; this uses 
   * the fact that datacons have been sorted by name. 
   *)
  fun compareDcons(spec,actual) =
    let fun comp(l1 as dc1::r1, l2 as dc2::r2, s_only, a_only) =
              if S.eq(dc1,dc2) then comp(r1,r2,s_only,a_only)
              else if S.symbolGt(dc1,dc2) 
                   then comp(l1,r2,s_only,dc2::a_only)
                   else comp(r1,l2,dc1::s_only,a_only)

          | comp([], [], s_only, a_only) = (rev s_only, rev a_only)
          | comp([], r, s_only, a_only)  = (rev s_only, rev a_only @ r)
          | comp(r, [], s_only, a_only)  = (rev s_only @ r, rev a_only)
     in comp(spec,actual,[],[])
    end

  fun checkTycBinding(_,T.ERRORtyc,_) = ()
    | checkTycBinding(specTycon,strTycon,entEnv) =
    let val specName = S.name(TU.tycName specTycon)
     in case specTycon
         of GENtyc {stamp=s,arity,kind=specKind,eq=ref eqprop,...} => let
                fun no_datatype () =
                    complain'("type "^specName^" must be a datatype")
            in
              if arity <> TU.tyconArity strTycon
              then complain' ("tycon arity for " ^ specName
                              ^ " does not match specified arity")
              else (case (specKind, (* TU.unWrapDefStar *) strTycon)
                       (* BUG: under certain circumstances (bug 1364), a DEFtyc
                        * strTycon should not be unwrapped.  However, it
                        * must be unwrapped if it is a DEFtyc created by
                        * instantiating a direct or indirect datatype
                        * replication spec (see bug 1432).
                        * For direct datatype replication {\em declarations},
                        * there is no problem because the replicated
                        * datatype is a GENtyc.
                        * The unwrapping of datatype relicants should be
                        * performed in Instantiate, not here.
                        *)
                     of (DATATYPE{index,family={members,...},...},
                         GENtyc {arity=a',kind,...}) =>
                     (case kind of
                          DATATYPE{index=index', family={members=members',...},
                                   ...} =>
                          let val specDconSig = #dcons(Vector.sub(members,index))
                              val strDconSig = #dcons(Vector.sub(members',index'))
                              val specnames = map #name specDconSig
                              val strnames = map #name strDconSig

                              val _ = app (fn s => 
                                              (debugmsg (S.name s))) specnames
                              val _ = debugmsg "******"
                              val _ = app (fn s => 
                                              (debugmsg (S.name s))) strnames

                          in
                              case compareDcons (specnames, strnames)
                               of ([],[]) => ()
                                | (s_only, a_only) =>
                                  complain'(concat(List.concat
                                      [["datatype ",specName,
                                        " does not match specification"],
                                       case s_only
                                        of [] => []
                                         | _  => 
                                            ["\n   constructors in spec only: ",
                                             symbolsToString s_only],
                                       case a_only
                                        of [] => []
                                         | _  => 
                                            ["\n   constructors in actual only: ",
                                             symbolsToString a_only]]))
                          end
                        | _ => no_datatype ())
                      | (DATATYPE _, _) => no_datatype ()
                      | (FORMAL, _) =>
                           if eqprop=YES andalso not(EqTypes.isEqTycon strTycon)
                           then complain'("type " ^ specName ^
                                          " must be an equality type")
                           else ()
                      | _ =>
                           (debugPrint(debugging)("specTycon: ",
                            PPType.ppTycon statenv, specTycon);
                            debugPrint(debugging)("strTycon: ",
                            PPType.ppTycon statenv, strTycon);
                            bug "checkTycBinding 1" ))
            end
          | DEFtyc{tyfun=TYFUN{arity,body},strict,stamp,path} => 
              let val ntyfun = TYFUN{arity=arity,body=MU.transType entEnv body}
                  val specTycon' = DEFtyc{tyfun=ntyfun,strict=strict,
                                          stamp=stamp,path=path}
               in if TU.equalTycon(specTycon',strTycon)
                  then ()
                  else (debugPrint(debugging)("specTycon': ",
                          PPType.ppTycon statenv, specTycon);
                        debugPrint(debugging)("strTycon: ",
                          PPType.ppTycon statenv, strTycon);
                        complain'("type " ^ specName ^
                                  " does not match definitional specification"))
              end
          | ERRORtyc => raise BadBinding
          | _ => bug "checkTycBinding 2"
    end

  (*** lookStr is only used inside the checkSharing function ***)
  fun lookStr (elements,entEnv) (SP.SPATH spath) : (M.Signature * M.entity) =
    let fun loop ([sym],elements,entEnv) =
              ((case MU.getSpec(elements,sym)
                 of STRspec{entVar,sign,...} =>
                     (debugmsg ("$lookStr.1: "^S.name sym^", "^EP.entVarToString entVar);
                     (sign,EE.look(entEnv,entVar)))
                  | _ => bug "looStr 1b")
                handle MU.Unbound _ => bug "lookStr 1c")

          | loop (sym::rest,elements,entEnv) =
              ((case MU.getSpec(elements,sym)
                 of STRspec{sign=SIG{elements,...},entVar,...} =>
                      (case EE.look(entEnv,entVar)
                        of STRent {entities,...} =>
                           (debugmsg ("$lookStr.2: "^S.name sym^", "^
                                      EP.entVarToString entVar);
                            loop(rest,elements,entities))
                         | ERRORent => (ERRORsig,ERRORent)
                         | _ => bug "lookStr 2a")
                  | _ => bug "lookStr 2b")
                handle MU.Unbound _ => bug "lookStr 2c")

          | loop _ = bug "lookStr 3"
     in loop(spath,elements,entEnv)
    end

  (*** lookTyc is only used inside the checkSharing function ***)
  fun lookTyc (elements,entEnv) (SP.SPATH spath) : T.tycon =
    let fun loop ([sym],elements,entEnv) =
              ((case MU.getSpec(elements,sym)
                 of TYCspec{entVar,...} =>
                      (case EE.look(entEnv,entVar)
                        of TYCent tycon => tycon
                         | ERRORent => ERRORtyc
                         | _ => bug "lookTyc 1a")
                  | _ => bug "looTyc 1b")
                handle MU.Unbound _ => bug "lookTyc 1c")

          | loop (sym::rest,elements,entEnv) =
              ((case MU.getSpec(elements,sym)
                 of STRspec{sign=SIG{elements,...},entVar,...} =>
                   (case EE.look(entEnv,entVar)
                     of STRent {entities,...} => loop(rest,elements,entities)
                      | ERRORent => ERRORtyc
                      | _ => bug "lookTyc 2a")
                  | _ => bug "lookTyc 2b")
               handle MU.Unbound _ => bug ("lookTyc 2c:"^Symbol.name sym^
                                           SP.toString(SP.SPATH spath)))

          | loop _ = bug "lookTyc 3"
     in loop(spath,elements,entEnv)
    end

  (*** verify whether all the sharing constraints are satisfied ***)
  fun checkSharing(sign as ERRORsig, entEnv) = ()
        (* don't do anything if an error has occured, resulting in an ERRORsig *)
    | checkSharing(sign as SIG{elements,typsharing,strsharing,...}, entEnv) =
        let fun errmsg sp x = SP.toString x ^ " # " ^ SP.toString sp

            fun eqTyc(_,ERRORtyc) = true
              | eqTyc(ERRORtyc,_) = true
              | eqTyc(tyc1,tyc2) = TU.equalTycon(tyc1,tyc2)

            val lookStr = lookStr (elements,entEnv)


            fun commonElements(SIG sg1, SIG sg2) =
                let val elems1 = #elements sg1
                    val elems2 = #elements sg2
                    fun elemGt ((s1,_),(s2,_)) = S.symbolGt(s1,s2)
                    val elems1 = ListMergeSort.sort elemGt elems1
                    val elems2 = ListMergeSort.sort elemGt elems2
                    fun intersect(e1 as ((s1,spec1)::rest1),
                                  e2 as ((s2,spec2)::rest2)) =
                        if S.eq(s1,s2) then (s1,spec1,spec2)::intersect(rest1,rest2)
                        else if S.symbolGt(s1,s2) then intersect(e1,rest2)
                        else intersect(rest1,e2)
                      | intersect(_,_) = nil
                 in intersect(elems1,elems2)
                end
              | commonElements _ = bug "commonElements"


            fun appPairs test nil = ()
              | appPairs test (a::r) =
                  (app (fn x => test(a,x)) r; appPairs test r)

            fun compStr((p1,(sign1,ent1)),
                        (p2,(sign2,ent2))) = 
                 (case (ent1,ent2)
                    of (STRent {stamp = s1, entities = ee1, ... },
                        STRent {stamp = s2, entities = ee2, ... }) =>
                       if ST.eq(s1,s2) then () (* shortcut! *)
                       else if MU.eqSign(sign1,sign2) then
                           let val _ = debugmsg "$compStr: equal signs"
                               val { elements, ... } =
                                   case sign1 of SIG sg => sg
                                               | _ => bug "compStr:SIG"
                           in for elements (fn 
                                  (sym,TYCspec{entVar,...}) => 
                                  let val tyc1 =
                                          unTYCent (EE.look(ee1,entVar))
                                      val tyc2 =
                                          unTYCent (EE.look(ee2,entVar))
                                  in if eqTyc(tyc1,tyc2) then ()
                                     else complain(
                                       concat["implied type sharing violation: ",
                                              errmsg (SP.extend(p1,sym))
                                                     (SP.extend(p2,sym))])
                                  end
                                | (sym,STRspec{entVar,sign,...}) => 
                                  let val ent1' = EE.look(ee1,entVar)
                                      val ent2' = EE.look(ee2,entVar)
                                  in compStr((SP.extend(p1,sym),(sign,ent1')),
                                             (SP.extend(p2,sym),(sign,ent2')))
                                  end
                                | _ => ())
                           end
                       else
                           let val _ = debugmsg "$compStr: unequal signs"
                               val common = commonElements(sign1,sign2)
                           in for common (fn 
                                (sym,TYCspec{entVar=v1,...},TYCspec{entVar=v2,...}) =>
                                let val tyc1 = unTYCent (EE.look(ee1,v1))
                                    val tyc2 = unTYCent (EE.look(ee2,v2))
                                in if eqTyc(tyc1,tyc2) then ()
                                   else complain(
                                             concat["type sharing violation: ",
                                                    errmsg (SP.extend(p1,sym))
                                                           (SP.extend(p2,sym))])
                                end
                              | (sym,STRspec{entVar=v1,sign=sign1',...},
                                 STRspec{entVar=v2,sign=sign2',...}) =>
                                let val str1 = EE.look(ee1,v1)
                                    val str2 = EE.look(ee2,v2)
                                in compStr((SP.extend(p1,sym),(sign1',str1)),
                                           (SP.extend(p2,sym),(sign2',str2)))
                                end
                              | _ => ()) (* values, constructors, functors *)
                           end
                     | (ERRORent,_) => ()  (* error upstream *)
                     | (_,ERRORent) => () (* error upstream *)
                     | _ => bug "compStr")

            fun checkStr (paths) =
                let val pathstrs = map (fn p => (p,lookStr p)) paths
                 in appPairs compStr pathstrs
                end

            fun checkTyc0 (firstPath, rest) =
              let val lookTyc = lookTyc (elements,entEnv)
                  val errMsg = errmsg firstPath
                  val first = lookTyc firstPath
                  fun checkPath p = 
                      if eqTyc(first, lookTyc p) then ()
                      else complain(concat["type sharing violation: ",errMsg p])
               in app checkPath rest
              end

            fun checkTyc (sp::rest) = checkTyc0(sp,rest)
              | checkTyc _ = bug "checkSharing:checkTyc"

         in app checkStr strsharing;
            app checkTyc typsharing
        end

  (* 
   * Matching: Go through the `elements' of the specified signature, and 
   * construct a corresponding realization from entities found in the given 
   * structure.  The structure's entities are found by using the entPath in 
   * each of the given structure signature's elements to access the given 
   * structure's realization = stored entEnv.  Recurse into substructures.  
   * Build the formal realization in parallel.  Finally check sharing 
   * constraints.
   *)

  (* 
   * val matchElems : 
   *      (S.symbol * spec) list * entEnv * entityDec list * A.dec list 
   *       * B.binding list * bool
   *       -> (entEnv * entityDec list * A.dec list * B.binding list * bool)
   *
   * Given the elements and the entities of a structure S, and a spec
   * from a signature, extend the realization (entityEnv) with the
   * entity specified by the spec, extend the list of
   * coercions (entity declarations) with a declaration which
   * will evaluate to the newly created entity, and extend the thinning.
   *
   * Assumption: if a match error occurs, then the resulting thinning
   * and the list of entityDecs will never be used -- they will not be
   * well-formed in case of errors. 
   *)

  fun matchDefStr0(sigElements,signD,rlznD,signM,rlznM) =
      let val dropVals = List.filter
               (fn (s,(TYCspec _ | STRspec _ )) => true | _ => false)
          fun elemGt ((s1,_),(s2,_)) = S.symbolGt(s1,s2)
          val commonDM =
              if MU.eqSign(signD,signM) then
                let val { elements = elems, ... } =
                        case signD of SIG sg => sg
                                    | _ => bug "matchDefStr0:SIG(1)"
                    val elems = ListMergeSort.sort elemGt (dropVals elems)
                 in map (fn (s,spec) => (s,spec,spec)) elems
                end
              else
                let val { elements = elemsD, ...} =
                        case signD of SIG sg => sg
                                    | _ => bug "matchDefStr0:SIG(2)"
                    val { elements = elemsM, ...} =
                        case signM of SIG sg => sg
                                    | _ => bug "matchDefStr0:SIG(3)"
                    val elemsD = ListMergeSort.sort elemGt (dropVals elemsD)
                    val elemsM = ListMergeSort.sort elemGt (dropVals elemsM)
                    fun intersect(e1 as ((s1,spec1)::rest1),
                                  e2 as ((s2,spec2)::rest2)) =
                        if S.eq(s1,s2) then (s1,spec1,spec2)::intersect(rest1,rest2)
                        else if S.symbolGt(s1,s2) then intersect(e1,rest2)
                        else intersect(rest1,e2)
                      | intersect(_,_) = nil
                 in intersect(elemsD,elemsM)
                end
          val sigElements' = ListMergeSort.sort elemGt (dropVals sigElements)
          fun intersect'(elems1 as ((sym1,x)::rest1),
                         elems2 as ((sym2,y,z)::rest2)) =
              if S.eq(sym1,sym2) then
                (sym1,x,y,z)::intersect'(rest1,rest2)
              else if S.symbolGt(sym1,sym2) then
                intersect'(elems1,rest2) (* discard sym2 *)
              else intersect'(rest1,elems2) (* discard sym1 *)
            | intersect'(_,_) = nil
          val common = intersect'(sigElements',commonDM)
          fun loop nil = true
            | loop ((sym,spec,specD,specM)::rest) =
              (case spec
                 of (TYCspec _ ) =>
                     let fun unTYCspec (TYCspec{entVar,...}) =
                               entVar
                           | unTYCspec _ = bug "matchStr:unTYCspec"
                         val evD = unTYCspec specD
                         val evM = unTYCspec specM
                         val {entities=eeD,...} = rlznD
                         val {entities=eeM,...} = rlznM
                         val tycD = unTYCent (EE.look(eeD,evD))
                         val tycM = unTYCent (EE.look(eeM,evM))
			 val speceq = TU.equalTycon(tycD,tycM) 
		     in (if speceq then speceq 
			 else 
			     (withInternals
				  (fn() => 
				     (debugPrint(debugging)
					("Tycon mismatch def: ",
					 PPType.ppTycon statenv, tycD); 
				      debugPrint(debugging)
					("Tycon mismatch mod: ",
					 PPType.ppTycon statenv, tycM))); 
			      speceq)) andalso loop rest
                     end
                  | STRspec{sign=SIG {elements,...},...} =>
                     let fun unSTRspec (STRspec x) = x
                           | unSTRspec _ = bug "strMatch:unSTRspec"
                         val {entVar=evD,sign=signD',...} = unSTRspec specD
                         val {entVar=evM,sign=signM',...} = unSTRspec specM
                         val {entities=eeD,...} = rlznD
                         val {entities=eeM,...} = rlznM
                     in (case (EE.look(eeD,evD), EE.look(eeM,evM)) 
			  of ((ERRORent, _) | (_, ERRORent)) =>
			     matchDefStr0(elements,signD,rlznD,signM,rlznM)
			     andalso loop rest
			   | (STRent rlznD', STRent rlznM') =>
			     matchDefStr0(elements,signD',rlznD',signM',
					  rlznM')
			     andalso loop rest
			   | _ => bug "strMatch:matchDefStr0")
                     end
                  | _ => bug "matchStr") 
       in loop common
      end

  fun matchDefStr (sigElements, STR {sign=signD,rlzn=rlznD,...},
                                STR {sign=signM,rlzn=rlznM,...}) =
      let val sD = #stamp rlznD
          val sM = #stamp rlznM
      in
          if ST.eq(sD,sM) (* eqOrigin *)
          then true
          else matchDefStr0(sigElements,signD,rlznD,signM,rlznM)
      end
    | matchDefStr((_, ERRORstr, _) | (_, _, ERRORstr)) = true 
    | matchDefStr _ = bug "matchDefStr (2)"

  fun matchElems ([], entEnv, entDecs, decs, bindings, succeed) =
        (entEnv, rev entDecs, rev decs, rev bindings, succeed)

    | matchElems ((sym, spec) :: elems, entEnv, entDecs, decs, bindings, succeed) =

       let val _ = debugmsg ">>matchElems"
           fun matchErr (kindOp: string option) =
             let val entEnv' = 
                   case MU.getSpecVar spec
                    of SOME v => EE.bind(v, ERRORent, entEnv)
                     | NONE => entEnv

                 (* synthesize a new error binding to remove improper error
                    messages on inlInfo (ZHONG)
                 val bindings' = 
                   case spec
                     of TYCspec _ => bindings
                      | CONspec {slot=NONE, ...} => bindings
                      | _ => B.CONbind VarCon.bogusEXN :: bindings
                 -- assume this is no longer relevant, since inlInfo is gone (DBM) *) 
              in case kindOp
                   of SOME kind =>
                        complain("unmatched " ^ kind ^ " specification: " ^ S.name sym)
                    | NONE => ();
                 matchElems(elems, entEnv', entDecs, decs, bindings, false)
             end

           fun typeInMatched (kind,typ) = 
                 (MU.transType entEnv typ) 
                    handle EE.Unbound => 
                      (debugPrint (debugging) (kind, PPType.ppType statenv,typ);
                       raise EE.Unbound)

           fun typeInOriginal (kind,typ) = 
                 (MU.transType strEntEnv typ) 
                    handle EE.Unbound => 
                      (debugPrint (debugging) (kind, PPType.ppType statenv,typ);
                       raise EE.Unbound)

        in case spec
            of TYCspec{entVar,info=RegTycSpec{spec=specTycon,repl,scope}} =>
                (let val _ = debugmsg(String.concat[">>matchElems TYCspec: ",
                                                    S.name sym, ", ",
                                                    ST.toString entVar])
                     val (strTycon, strEntVar) =
                            MU.getTyc(strElements, strEntEnv, sym)
                            handle EE.Unbound =>
                              (debugPrint(debugging) ("strEntEnv: ", 
                                (fn pps => fn ee => 
                                   PPModules.ppEntityEnv pps (ee,statenv,6)),
                                strEntEnv); raise EE.Unbound)

                     val _ = debugmsg ("--matchElems TYCspec - strEntVar: "^
                                       ST.toString strEntVar)

                     (*** DAVE: please check the following ! ***)
                     val tycEntExp = 
                       case epath of [] => CONSTtyc strTycon
                                   | _ => VARtyc(rev(strEntVar::epath))

                     val _ = debugmsg "--matchElems TYCspec >> checkTycBinding"
                     val _ = checkTycBinding(specTycon, strTycon, entEnv)
                     val entEnv' = EE.bind(entVar, TYCent strTycon, entEnv) 
                     val entDecs' = TYCdec(entVar, tycEntExp) :: entDecs
                     val _ = debugmsg "<<matchElems TYCspec << checkTycBinding"

                  in matchElems(elems, entEnv', entDecs', decs, bindings, succeed)
                 end handle MU.Unbound sym => matchErr (SOME "type")
                          | BadBinding => matchErr NONE
                          | EE.Unbound =>
                              (debugmsg ("$matchElems(TYCspec): "^S.name sym);
                               raise EE.Unbound))

             | STRspec{sign=thisSpecSig as SIG sg, entVar, def, ...} =>
                (let val thisElements = #elements sg
                     val _ = debugmsg(String.concat["--matchElems STRspec: ",
                                                    S.name sym,", ",
                                                    ST.toString entVar])
                     val (strStr, strEntVar) = 
                       MU.getStr(strElements, strEntEnv, sym, rootAcc, rootPrim)

                     (* verify spec definition, if any *)
                       (* matchDefStr now does the proper deep, component-wise
                        * comparison of specStr and strStr when their stamps
                        * don't agree, but the error message printed
                        * when definition spec is not matched leaves something
                        * to be desired *)
                     val _ = 
                         case def
                           of NONE => ()
                            | SOME(sd,_) =>
                               let val specStr = MU.strDefToStr(sd,entEnv) 
                                in if matchDefStr(thisElements,specStr,strStr) then ()
                                   else 
                                    (case sd
                                      of M.VARstrDef(sign,ep) =>
                                          debugmsg("spec def VAR: "^
                                           EP.entPathToString ep ^ "\n")
                                       | M.CONSTstrDef _ =>
                                          debugmsg("spec def CONST\n");
                                     showStr("specStr: ", specStr);
                                     showStr("strStr:  ", strStr);
                                     complain("structure def spec for "^
                                              S.name sym ^ " not matched"))
                               end

                     val epath' = strEntVar::epath
                     val rpath' = IP.extend(rpath, sym)
                     val (thinDec, thinStr, strExp) = 
                       matchStr1(thisSpecSig, strStr, sym, entEnv, epath',
                                 rpath', statenv, region, compInfo)

                     val entEnv' = 
                       let val strEnt = 
                             case thinStr of M.STR { rlzn, ... } => rlzn
                                           | _ => M.bogusStrEntity
                        in EE.bind(entVar, M.STRent strEnt, entEnv)
                       end

                     val entDecs' = M.STRdec(entVar, strExp, sym) :: entDecs 
                     val decs' = thinDec :: decs
                     val bindings' = (B.STRbind thinStr)::bindings
                     val succeed = (case thinStr
                                      of M.ERRORstr => false
                                       | _ => succeed)
                  in matchElems(elems, entEnv', entDecs', decs', bindings', succeed)
                 end handle MU.Unbound sym => matchErr (SOME "structure"))

             | FCTspec{sign=specSig, entVar, ...} => 
                (let val _ = debugmsg(String.concat["--matchElems FCTspec: ",
                                                    S.name sym,", ",
                                                    ST.toString entVar])

                     val (strFct, fctEntVar) = 
                       MU.getFct(strElements, strEntEnv, sym, rootAcc, rootPrim)
                     val exp' = M.VARfct(rev(fctEntVar::epath))
                     val rpath' = IP.extend(rpath,sym)
                     val (thinDec, thinFct, fctExp) = 
                       matchFct1(specSig, strFct, sym, entEnv, exp', 
                                 rpath', statenv, region, compInfo)

                     val entEnv' = 
                       let val fctEnt = 
                             case thinFct of M.FCT { rlzn, ... } => rlzn
                                           | _ => M.bogusFctEntity
                        in EE.bind(entVar, M.FCTent fctEnt, entEnv)
                       end

                     val entDecs' = M.FCTdec(entVar, fctExp) :: entDecs 
                     val decs' = thinDec :: decs
                     val bindings' = (B.FCTbind thinFct)::bindings
                     val succeed = (case thinFct
                                      of M.ERRORfct => false
                                       | _ => succeed)

                  in matchElems(elems, entEnv', entDecs', decs', bindings', succeed)
                 end handle MU.Unbound sym => matchErr(SOME "functor"))

             | VALspec{spec=spectyp, ...} => 
                ((case (MU.getSpec(strElements, sym))
                   of VALspec{spec=acttyp, slot=actslot} =>
                     let 
                         val _ =
                             (debugPrint debugging
                                ("spectype[0]", PPType.ppType statenv,
                                 spectyp);
                              debugPrint debugging 
                                ("acttyp[0]", PPType.ppType statenv,
                                  acttyp))

                         val spectyp = typeInMatched("$specty(val/val)", spectyp)
                         val acttyp = typeInOriginal("$actty(val/val)", acttyp)
                         val dacc = DA.selAcc(rootAcc, actslot)
                         val prim = PrimOpId.selValPrimFromStrPrim(rootPrim, actslot)

                         val _ =
                             (debugPrint debugging
                                ("spectype[1]", PPType.ppType statenv,
                                 spectyp);
                              debugPrint debugging 
                                ("acttyp[1]", PPType.ppType statenv,
                                  acttyp))

                     in case matchTypes(spectyp, acttyp, sym)
                          of NONE => matchErr NONE
                           | SOME (btvs,ptvs) =>
                             let val _ =
                                 (debugmsg "###SM: "; 
                                  debugmsg (S.name sym); 
                                  debugmsg "\n";
                                  debugPrint debugging
                                    ("spectype", PPType.ppType statenv,
                                     spectyp);
                                  debugPrint debugging 
                                    ("acttyp", PPType.ppType statenv,
                                      acttyp);
                                  debugPrint debugging
                                    ("ptvs",
                                     (fn pps =>
                                         PU.ppTuple pps 
                                           (fn pps => (fn tv => 
                                              PPType.ppType statenv pps
                                                            (T.VARty tv)))),
                                     ptvs);
                                  debugPrint debugging
                                    ("btvs",
                                     (fn pps =>
                                         PU.ppTuple pps 
                                           (fn pps => (fn tv =>
                                              PPType.ppType statenv pps
                                                            (T.VARty tv)))),
                                     btvs);
                                  debugmsg "\n")

                                 val spath = SP.SPATH[sym]
                                 val actvar = VALvar{path=spath, typ=ref acttyp,
                                             access=dacc, prim=prim,
					     btvs = ref []}
			         (* spectyp may be polymorphic even if 
				    acttyp is monomorphic (for nonstrict
				    type operators), see modules test case 309 *)
                                 val (decs', nv) = 
                                     case (ptvs, btvs)
                                       of ([],[]) => (* acttyp & spectyp mono *)
					  (decs, actvar) 
                                        | _ =>
                                          let val acc = DA.namedAcc(sym, mkv)
                                              val specvar = 
                                                VALvar{path=spath, typ=ref spectyp,
                                                       access=acc, prim=prim,
						       btvs = ref []}
                                              val vb = 
                                                A.VB {pat=A.VARpat specvar,
                                                      exp=A.VARexp(ref actvar, ptvs),
                                                      boundtvs=btvs, tyvars=ref []}
                                           in ((A.VALdec [vb])::decs, specvar)
                                          end

                                 val bindings' = (B.VALbind nv)::bindings

                              in matchElems(elems, entEnv, entDecs,
                                            decs', bindings', succeed)
                             end
                     end

                    | CONspec{spec=DATACON{typ=acttyp, name, const,
                                           rep, sign, lazyp}, slot} => 
                     let val spectyp = typeInMatched("$specty(val/con)", spectyp)
                         val acttyp = typeInOriginal("$actty(val/con)", acttyp)
                      in case matchTypes(spectyp, acttyp, name)
                           of NONE => matchErr NONE
                            | SOME(boundtvs,paramtvs) =>
                              let val nrep = 
                                      case slot 
                                       of SOME s => exnRep(rep, DA.selAcc(rootAcc, s))
                                        | NONE => rep

                                  val (decs', bindings') =
                                      let val con = 
                                              DATACON{typ=acttyp, name=name, const=const, 
                                                      rep=nrep, sign=sign, lazyp=lazyp}
                                          val acc = DA.namedAcc(name, mkv)
                                          val specvar = 
                                              VALvar{path=SP.SPATH[name], access=acc,
                                                     prim=PrimOpId.NonPrim,
						     btvs = ref [],
                                                     typ=ref spectyp}
                                          val vb = 
                                              A.VB {pat=A.VARpat specvar,
                                                    exp=A.CONexp(con, paramtvs),
                                                    boundtvs=boundtvs, tyvars=ref []}
                                      in ((A.VALdec [vb])::decs, 
                                          (B.VALbind specvar)::bindings)
                                      end
                              in matchElems(elems, entEnv, entDecs, decs', 
                                            bindings', succeed)
                              end
                     end
                 | _ => bug "matchVElem.1")
               handle MU.Unbound sym => matchErr(SOME "value"))

             | CONspec{spec=DATACON{name, typ=spectyp, lazyp,
                                    rep=specrep, ...},...} => 
               ((case MU.getSpec(strElements, sym)
                  of CONspec{spec=DATACON{typ=acttyp, rep=actrep, const, 
                                        sign, ...}, slot} =>
                     if (DA.isExn specrep) = (DA.isExn actrep) then
                     let val spectyp = typeInMatched("$specty(con/con)", spectyp)
                         val acttyp = typeInOriginal("$actty(con/con)", acttyp)
                      in case matchTypes(spectyp, acttyp, name)
                           of NONE => matchErr NONE
                            | _ => 
                              let
                                  val bindings' =
                                    case slot 
                                     of NONE => bindings 
                                      | SOME s => 
                                          let val dacc = DA.selAcc(rootAcc, s)
                                              val nrep = exnRep(actrep, dacc) 
                                              val con = DATACON{typ=acttyp, name=name,
                                                                const=const, rep=nrep,
                                                                sign=sign, lazyp=lazyp}
                                           in (B.CONbind con) :: bindings
                                          end

                               in matchElems(elems, entEnv, entDecs, decs,
                                             bindings', succeed)
                              end
                     end
                     else raise MU.Unbound sym

                   | VALspec _ =>
                     if DA.isExn specrep then matchErr(SOME "exception")
                     else matchErr(SOME "constructor")
                   | _ => bug "matchVElem.2")
                handle MU.Unbound sym => 
                       if DA.isExn specrep then matchErr(SOME "exception")
                       else matchErr(SOME "constructor"))
	       (* [GK 5/7/07] Try to avoid secondary error. Keep matching after
		  this ERRORspec. *)
	     | ERRORspec => matchElems(elems, entEnv, entDecs, decs, bindings, false)
       end (* function matchElems *)

  fun matchIt entEnv = 
    let val _ = debugmsg ">>matchIt"

        val (resultEntEnv, entDecs, absDecs, bindings, succeed) = 
            matchElems(sigElements, entEnv, [], [], [], true)
            handle EE.Unbound => (debugmsg "$matchIt 1"; raise EE.Unbound)
     in if succeed then
          let val resultEntEnv = EE.mark(mkStamp, resultEntEnv)
              val _ = debugmsg "--matchIt: elements matched successfully"

              val _ = checkSharing(specSig, resultEntEnv)
                      handle EE.Unbound => (debugmsg "$matchIt 3"; raise EE.Unbound)
              val _ = debugmsg "--matchIt: sharing checked"

              val resStr =
                let val strEnt = {stamp = strStamp,
                                  entities = resultEntEnv, 
                                  rpath=rpath,
                                  stub = NONE,
                                  properties = PropList.newHolder ()}
                    val dacc = DA.newAcc(mkv)
                    val dinfo = MU.strPrimElemInBinds bindings
                in M.STR {sign=specSig, rlzn=strEnt, access=dacc, prim=dinfo}
                end

              val resDec = 
                let val body = A.LETstr(A.SEQdec absDecs, A.STRstr bindings)
                 in A.STRdec [A.STRB{name=strName, str=resStr, def=body}]
                end

              val resExp = M.STRUCTURE{stamp = GETSTAMP(M.VARstr(rev epath)),
                                       entDec = SEQdec(entDecs)}

              val _ = debugmsg "<<matchIt"
           in (resDec, resStr, resExp) 
          end
        else (A.SEQdec[],ERRORstr,M.CONSTstr(M.bogusStrEntity))
    end

in 

(* we should not do such short-cut matching because we need to
   recalculuate the tycpath information for functor components.
   But completely turning this off is a bit too expensive, so 
   we add a fctflag in the signature to indicate whether it 
   contains functor components. 
   DBM: but we are no longer calculating tycpath information
   here. Do we still need the fctflag?
*)
    if (ST.eq(sigStamp, strSigStamp)) andalso closed andalso (not fctflag)
    then (A.SEQdec [], str, M.VARstr (rev epath))    (* short-cut matching *)
    else matchIt (if closed then EE.empty else matchEntEnv)
end
| matchStr1 _ = (A.SEQdec [], ERRORstr, bogusStrExp)
(* end of matchStr1 *)


(***************************************************************************
 * val matchStr :                                                          *
 *                                                                         *
 *     {sign     : Modules.Signature,                                      *
 *      str      : Modules.Structure,                                      *
 *      strExp   : Modules.strExp,                                         *
 *      entvar   : EntPath.entVar,                                         * 
 *      entEnv   : Modules.entityEnv,                                      *
 *      rpath    : InvPath.path,                                           *
 *      statenv  : StaticEnv.staticEnv,                                    *
 *      region   : SourceMap.region,                                       *
 *      compInfo : ElabUtil.compInfo} -> {resDec : Absyn.dec,              *
 *                                        resStr : Modules.Structure,      *
 *                                        resExp : Modules.strExp}         *
 *                                                                         *
 ***************************************************************************)
and matchStr {sign, str, strExp, entvar, entEnv, rpath, statenv, region,
              compInfo=compInfo as {mkStamp,...}: EU.compInfo} =

  let val _ = debugmsg ">>matchStr"

      val (resDec, resStr, exp) = 
          matchStr1 (sign, str, anonSym, entEnv, [entvar], rpath, 
                     statenv, region, compInfo)

      val resExp = M.CONSTRAINstr{boundvar=entvar, raw=strExp, coercion=exp}
(*    val resExp = M.LETstr(M.STRdec(entvar, strExp), exp) *)
(*    val resExp = M.APPLY(M.LAMBDA{param=entvar, body=exp}, strExp) *)
      val _ = debugmsg "<<matchStr"

   in {resDec=resDec, resStr=resStr, resExp=resExp}
  end 
  handle EE.Unbound => (debugmsg "$matchStr"; raise EE.Unbound)


(***************************************************************************
 *                                                                         *
 * Matching a functor against a functor signature:                         *
 *                                                                         *
 *  val matchFct1 : fctSig * Functor * S.symbol *                          *
 *                  entityEnv * M.fctExp * IP.path * staticEnv *           *
 *                  region * EU.compInfo                                   *
 *                  -> A.dec * M.Functor * M.fctExp                        *
 *                                                                         *
 *  Arguments: funsig  F(fsigParVar : fsigParSig) = fsigBodySig            *
 *             functor F(fctParVar : fctParSig) : fctBodySig = bodyExp     *
 *                                                                         *
 *  Result:    functor F(fctParVar : fctParSig) : fctBodySig = resBodyExp  *
 *                                                                         *
 ***************************************************************************)
and matchFct1(specSig as FSIG{paramsig=fsigParamSig,paramvar=fsigParamVar,
                              paramsym,bodysig=fsigBodySig,...},
              fct as FCT { rlzn = fctRlzn, ... }, fctName : S.symbol,
              entEnv, uncoercedFct, rpath, statenv, region,
              compInfo as {mkStamp, mkLvar=mkv,...}: EU.compInfo)
              : A.dec * M.Functor * M.fctExp =
(let 

(*** the entity var for the source functor "uncoercedFct" *)
val uncoerced = mkStamp()
val srcFctExp = M.VARfct [uncoerced]
val paramSym = case paramsym of SOME x => x 
                              | NONE => paramSym

(*** parameter signature instantiation for the functor signature being matched ***)
val {rlzn=fsigParEnt,...} = 
    INS.instFormal{sign=fsigParamSig, entEnv=entEnv,
                   rpath=IP.IPATH[paramSym], region=region, compInfo=compInfo}

val fsigParInst = 
  let val fsigParDacc = DA.newAcc(mkv)
   in M.STR{sign=fsigParamSig, rlzn=fsigParEnt, 
            access=fsigParDacc, prim=[]}
  end

(*** applying fct to the fsigParInst structure ***)
val paramId = fsigParamVar (* mkStamp() *)
val {resDec=resDec1, resStr=resStr1, resExp=resExp1} = 
  let val paramExp = M.VARstr [paramId]
      val entvar = mkStamp()
   in applyFct{fct=fct, fctExp=srcFctExp, argStr=fsigParInst, 
               argExp=paramExp, entvar=mkStamp(),
               epc=EPC.initContext (* ? ZHONG *), statenv=statenv, 
               rpath = IP.empty, region=region, compInfo=compInfo}
  end
          
(*** matching the result structure against the body sig ***) 
val fsigBodySigEnv = EE.bind(fsigParamVar, STRent fsigParEnt, entEnv)
val {resDec=resDec2, resStr=resStr2, resExp=resExp2} = 
  let val rp = IP.IPATH[S.strSymbol "<FctResult>"]
   in matchStr{sign=fsigBodySig, str=resStr1, strExp=resExp1, entvar=mkStamp(),
               entEnv=fsigBodySigEnv, rpath=rp, 
               statenv=statenv, region=region, compInfo=compInfo}
  end

(*** the resulting coerced functor ***)
val resFct =
    let val resExp3 = M.LETstr(M.FCTdec(uncoerced, M.CONSTfct fctRlzn), 
			       resExp2)
	val resClosure = CLOSURE{param=paramId, body=resExp3, env=entEnv}

	val resRlzn = {stamp = #stamp fctRlzn, (*** DAVE ? ***)
		       paramRlzn = fsigParEnt,
		       closure = resClosure, rpath=rpath,
		       stub = NONE,
		       properties = PropList.newHolder ()}

     in M.FCT{sign = specSig, rlzn = resRlzn,
	      access = DA.newAcc(mkv), prim = []}
    end

(*** the resulting functor absyn ***)
val fdec = 
    let val bodyAbs = A.LETstr(A.SEQdec [resDec1, resDec2], A.VARstr resStr2)
	val fctexp = A.FCTfct {param=fsigParInst, (* argtycs=paramTps,*) def=bodyAbs}
     in A.FCTdec [A.FCTB {name=anonFsym, fct=resFct, def=fctexp}]
    end

(*** the functor entity expression ***)
val fctExp = 
    M.LETfct(M.FCTdec(uncoerced, uncoercedFct), 
             M.LAMBDA{param = paramId, body = resExp2, paramRlzn=fsigParEnt})
    (* ??? is fsigParEnt the correct realization to use here? *)

in  (fdec, resFct, fctExp)

end handle Match => (A.SEQdec [], ERRORfct, bogusFctExp))
     (* 
      * This is intended to handle only the two left-hand side 
      * occurrences of STR{ ... } above, and is very crude. 
      * It should be replaced by case-expressions on the results of 
      * match etc. 
      *)

| matchFct1 _ =  (A.SEQdec [], ERRORfct, bogusFctExp)


(***************************************************************************
 *                                                                         *
 * val matchFct :                                                          *
 *                                                                         *
 *     {sign     : Modules.fctSig,                                         *
 *      fct      : Modules.Functor,                                        *
 *      fctExp   : Modules.fctExp,                                         *
 *      entEnv   : Modules.entityEnv,                                      *
 *      rpath    : InvPath.path,                                           *
 *      statenv  : StaticEnv.staticEnv,                                    *
 *      region   : SourceMap.region,                                       *
 *      compInfo : ElabUtil.compInfo} -> {resDec : Absyn.dec,              *
 *                                        resFct : Modules.Functor,        *
 *                                        resExp : Modules.fctExp}         *
 *                                                                         *
 ***************************************************************************)
and matchFct{sign, fct, fctExp, entEnv, rpath, 
             statenv, region, compInfo} = 
  let val _ = debugmsg ">>matchFct"

      val (resDec, resFct, resExp) = 
        matchFct1 (sign, fct, anonFsym, entEnv, fctExp, rpath, 
                   statenv, region, compInfo)

      val _ = debugmsg "<<matchFct"

   in {resDec=resDec, resFct=resFct, resExp=resExp}
  end 
  handle EE.Unbound => (debugmsg "$matchFct"; raise EE.Unbound)


(**************************************************************************
 *                                                                        *
 * Packing a structure against a signature:                               *
 *                                                                        *
 *  val packStr1 : Signature * strEntity * Structure * TU.tycset *        *
 *                 S.symbol * int * entityEnv * IP.path * staticEnv *     * 
 *                 region * EU.compInfo -> A.dec * M.Structure            *
 *                                                                        *
 **************************************************************************)
and packStr1(specSig as M.SIG {elements=sigElements,...},
	     resRlzn as {entities=resEntEnv,...},
	     str as M.STR {access=rootAcc,
			   rlzn=srcRlzn as {entities=srcEntEnv,...},
			   prim=rootPrim, ... },
             abstycs, strName, entEnv, rpath, statenv, region, 
             compInfo as {mkLvar=mkv, error, ...}: EU.compInfo) 
             : Absyn.dec * M.Structure =
let

fun typeInRes (kind,typ) = 
      (MU.transType resEntEnv typ) 
         handle EE.Unbound => 
           (debugPrint (debugging) (kind, PPType.ppType statenv, typ);
            raise EE.Unbound)

fun typeInSrc (kind,typ) = 
      (MU.transType srcEntEnv typ) 
         handle EE.Unbound => 
           (debugPrint (debugging) (kind, PPType.ppType statenv, typ);
            raise EE.Unbound)

fun packElems ([], entEnv, decs, bindings) = (rev decs, rev bindings)
  | packElems ((sym, spec) :: elems, entEnv, decs, bindings) = 
     let val _ = debugmsg ">>packElems"
      in case spec
          of STRspec{sign=thisSpecsig, entVar=ev, slot=s,...} =>
              (case (EE.look(resEntEnv, ev), EE.look(srcEntEnv, ev))  
                 of (M.STRent resStrRlzn, M.STRent srcStrRlzn) =>
		     let val srcStr = 
			     M.STR{sign=thisSpecsig, rlzn=srcStrRlzn,
				   access=DA.selAcc(rootAcc,s),
				    prim=PrimOpId.selStrPrimId(rootPrim,s)}
			 val rpath' = IP.extend(rpath, sym)
			 val (thinDec, thinStr) = 
			   packStr1(thisSpecsig, resStrRlzn, srcStr, abstycs,
				    sym, entEnv, rpath', statenv, 
				    region, compInfo)

                         val entEnv' = 
                           let val strEnt = 
                                case thinStr of M.STR { rlzn, ... } => rlzn
                                              | _ => M.bogusStrEntity
                            in EE.bind(ev, M.STRent strEnt, entEnv)
                           end

			 val decs' = thinDec :: decs
			 val bindings' = (B.STRbind thinStr) :: bindings

		      in packElems(elems, entEnv', decs', bindings')
		     end
	          | _ => (* missing element, error has occurred - do nothing *)
		    packElems(elems, entEnv, decs, bindings))

           | FCTspec{sign=thisSpecsig, entVar=ev, slot=s} => 
              (case (EE.look(resEntEnv, ev), EE.look(srcEntEnv, ev))
		 of (M.FCTent resFctRlzn, M.FCTent srcFctRlzn) =>
		     let val srcFct =
			     M.FCT {sign=thisSpecsig, rlzn=srcFctRlzn,
				    access=DA.selAcc(rootAcc,s),
				    prim=PrimOpId.selStrPrimId(rootPrim,s)}

			 val rpath' = IP.extend(rpath, sym)

			 val (thinDec, thinFct) = 
			   packFct1(thisSpecsig, resFctRlzn, srcFct, abstycs,
				    sym, entEnv, rpath', statenv,
				    region, compInfo)

                         val entEnv' = 
                           let val fctEnt = 
                                 case thinFct of M.FCT { rlzn, ... } => rlzn
                                               | _ => M.bogusFctEntity
                            in EE.bind(ev, M.FCTent fctEnt, entEnv)
                           end

			 val decs' = thinDec :: decs
			 val bindings' = (B.FCTbind thinFct) :: bindings

		      in packElems(elems, entEnv', decs', bindings')
		     end
		  | _ => packElems(elems, entEnv, decs, bindings))

           | VALspec{spec=spectyp, slot=s} => 
              (let val restyp = typeInRes("$spec-resty(packStr-val)", spectyp)
                   val srctyp = typeInSrc("$spec-srcty(packStr-val)", spectyp)
                   val dacc = DA.selAcc(rootAcc, s)
                   val prim = PrimOpId.selValPrimFromStrPrim(rootPrim, s)
(* dbm: assume that eqflag will always be true because of prior successful
 * sigmatch, therefore this does nothing ---
                   val (instys, btvs, resinst, eqflag) = 
                       absEqvTy(restyp, srctyp, prim)
*)
                   val spath = SP.SPATH[sym]
                   val srcvar = VALvar{path=spath, typ=ref srctyp,
		   		       btvs = ref[], access=dacc, prim=prim}

(* does nothing -- just use decs and srcvar below
                   val (decs', nv) = (decs, srcvar)
*)
(* dbm: was:
                   val (decs', nv) =
                     if eqflag then (decs, srcvar)
                     else (let val acc = DA.namedAcc(sym, mkv)
                               val resvar = 
                                 VALvar{path=spath, typ=ref restyp,
                                        access=acc, prim=PrimOpId.NonPrim}

                               val ntycs = TU.filterSet(resinst, abstycs)
                               val exp = 
                                 A.PACKexp(A.VARexp(ref srcvar, instys),
                                           resinst, ntycs)

                               val vb = A.VB {pat=(A.VARpat resvar), exp=exp,
                                              boundtvs=btvs, tyvars=ref []}

                            in ((A.VALdec [vb])::decs, resvar)
                           end)
*)

                   val bindings' = (B.VALbind srcvar)::bindings
                in packElems(elems, entEnv, decs, bindings')
               end)

           | CONspec{spec=DATACON{name, typ, rep, const, sign, lazyp}, slot} =>
              (let val bindings' =
                     case slot 
                      of NONE => bindings 
                       | SOME s => 
                           let val restyp = 
                                 typeInRes("$spec-resty(packStr-con)", typ)
                               val dacc = DA.selAcc(rootAcc, s)
                               val nrep = exnRep(rep, dacc) 
                               val con = DATACON{typ=restyp, name=name, lazyp=lazyp,
                                           const=const, rep=nrep, sign=sign}
                            in (B.CONbind(con)) :: bindings
                           end

                in packElems(elems, entEnv, decs, bindings')
               end)

           | TYCspec{entVar=ev,info=RegTycSpec{spec=specTycon,repl,scope}} =>
              (let val entEnv' = EE.bind(ev, EE.look(resEntEnv, ev), entEnv) 
                in packElems(elems, entEnv', decs, bindings)
               end)

           | _ => bug "packElems"
     end (* function packElems *)


val (absDecs, bindings) = packElems(sigElements, entEnv, [], [])       

val resStr =
  let val dacc = DA.newAcc(mkv)
      val dprim = MU.strPrimElemInBinds bindings
   in M.STR{sign=specSig, rlzn=resRlzn, access=dacc, prim=dprim}
  end

val resDec = 
  let val body = A.LETstr(A.SEQdec absDecs, A.STRstr bindings)
   in A.STRdec [A.STRB{name=strName, str=resStr, def=body}]
  end

in (resDec, resStr)
end 

| packStr1 _ = (A.SEQdec [], ERRORstr)


(***************************************************************************
 * Abstraction matching of a structure against a signature:                *
 *                                                                         *
 * val packStr :                                                           *
 *     {sign     : Modules.Signature,                                      * 
 *      str      : Modules.Structure,                                      * 
 *      strExp   : Modules.strExp,                                         *
 *      entEnv   : Modules.entityEnv,                                      * 
 *      rpath    : InvPath.path,                                           * 
 *      statenv  : StaticEnv.staticEnv,                                    * 
 *      region   : SourceMap.region,                                       * 
 *      compInfo : ElabUtil.compInfo} -> {resDec : Absyn.dec,              *
 *                                        resStr : Modules.Structure,      *
 *                                        resExp : Modules.strExp}         *
 *                                                                         *
 * INVARIANT: the base signature for str should be exactly sign; in other  *
 *            words, str should have been matched against sign before it   *
 *            being packed against sign.                                   *
 ***************************************************************************)
and packStr {sign, str, strExp, entEnv, rpath, 
             statenv, region, compInfo} = 
  let val _ = debugmsg ">>>packStr"

      val {rlzn=resRlzn, abstycs=abstycs, tyceps=_} = 
        let val srcRlzn = case str of M.STR { rlzn, ... } => rlzn
                                    | _ => M.bogusStrEntity
         in INS.instAbstr {sign=sign, entEnv=entEnv, srcRlzn=srcRlzn,
                           rpath=rpath, region=region, compInfo=compInfo}
        end
      val _ = debugmsg "packStr - instantiate done"

      val abstycs' = foldr TU.addTycSet (TU.mkTycSet()) abstycs

      val (resDec, resStr) = 
        packStr1 (sign, resRlzn, str, abstycs', anonSym,
                  entEnv, rpath, statenv, region, compInfo)
      val _ = debugmsg "packStr - packStr1 done"

      val resExp = M.ABSstr(sign, strExp)
      val _ = debugmsg "<<<packStr"

   in {resDec=resDec, resStr=resStr, resExp=resExp}
  end 


(***************************************************************************
 *                                                                         *
 * Packing a functor against a functor signature:                          *
 *                                                                         *
 *  val packFct1 : fctSig * fctEntity * Functor * tycon list *             *
 *                 S.symbol * entityEnv * IP.path * staticEnv * *
 *                 region * EU.compInfo -> A.dec * M.Functor               *
 *                                                                         *
 ***************************************************************************)
and packFct1(specSig as FSIG{paramsig, paramvar, bodysig, ...}, resFctRlzn,
             srcFct as FCT { rlzn = srcFctRlzn, ... },
             abstycs1, fctName, entEnv, rpath, statenv, region,
             compInfo as {mkStamp, mkLvar=mkv, error, ...}: EU.compInfo)
             : Absyn.dec * M.Functor = 

let

val {rlzn=paramEnt,...} = 
    INS.instFormal{sign=paramsig, entEnv=entEnv,
                   rpath=IP.IPATH[paramSym], region=region, compInfo=compInfo}

val paramStr = 
  let val paramDacc = DA.newAcc(mkv)
   in M.STR{sign=paramsig, rlzn=paramEnt, access=paramDacc,
	    prim=[]}
  end

val {resDec=rdec1, resStr=bodyStr, resExp=_} =
    applyFct{fct=srcFct, fctExp=CONSTfct srcFctRlzn, argStr=paramStr, 
             argExp=CONSTstr paramEnt, entvar=mkStamp(),
             epc=EPC.initContext (* ? ZHONG *), statenv=statenv, 
             rpath=IP.empty, region=region, compInfo=compInfo}

val bodyRlzn = 
  case bodyStr of M.STR { rlzn, ... } => rlzn
                | _ => M.bogusStrEntity

val {rlzn=resRlzn, abstycs=abstycs2, tyceps=_} = 
  let val entEnv' = 
        EE.mark(mkStamp, EE.bind(paramvar, STRent paramEnt, entEnv))
   in INS.instAbstr {sign=bodysig, entEnv=entEnv', srcRlzn=bodyRlzn,
                     rpath=rpath, region=region, compInfo=compInfo}
  end

val abstycs = foldr TU.addTycSet abstycs1 abstycs2

val (rdec2, resStr) = 
  let val rpath' = IP.IPATH[S.strSymbol "<FctResult>"]
   in packStr1(bodysig, resRlzn, bodyStr, abstycs, anonSym,
               entEnv, rpath', statenv, region, compInfo)
  end

val resFct = 
  let val resDacc = DA.newAcc(mkv)
   in M.FCT{sign=specSig, rlzn=resFctRlzn, access=resDacc, prim=[]}
  end

val resDec = 
  let val body = A.LETstr(rdec1, A.LETstr(rdec2, A.VARstr resStr))
      val fctexp = A.FCTfct{param=paramStr, def=body}
   in A.FCTdec [A.FCTB {name=fctName, fct=resFct, def=fctexp}]
  end

in (resDec, resFct)

end (* function packFct1 *)

| packFct1 _ =  (A.SEQdec [], ERRORfct)


(***************************************************************************
 * val applyFct :                                                          *
 *                                                                         *
 *     {fct      : Modules.Functor,                                        *
 *      fctExp   : Modules.fctExp,                                         *
 *      argStr   : Modules.Structure,                                      *
 *      argExp   : Modules.strExp,                                         *
 *      entvar   : EntPath.entVar,                                         *
 *      epc      : EntPathContext.context,                                 *
 *      statenv  : StaticEnv.staticEnv,                                    *
 *      rpath    : InvPath.path,                                           *
 *      region   : SourceMap.region,                                       *
 *      compInfo : ElabUtil.compInfo} -> {resDec : Absyn.dec,              *
 *                                        resStr : Modules.Structure,      *
 *                                        resExp : Modules.strExp}         *
 *                                                                         *
 * Match and coerce the argument and then do the functor application.      *
 * Returns the result structure, the result entity expression, and the     *
 * result abstract syntax declaration of resStr.                           *
 *                                                                         *
 * The argument matching takes place in the entityEnv stored in the        *
 * functor closure; this is where the paramsig must be interpreted.        *
 *                                                                         *
 ***************************************************************************)
and applyFct{fct as FCT {sign=FSIG (fsgn as {paramsig, bodysig, ...}),
			 rlzn = fctRlzn, access, prim},
             fctExp, argStr, argExp, entvar, epc,
             statenv, rpath, region,
             compInfo as {mkStamp, mkLvar=mkv, ...}} =
    let val {closure=CLOSURE {env=fctEntEnv, ... }, ... } = fctRlzn
	val _ = debugmsg ">>applyFct"

	(*** step #1: match the argument structure against paramSig ***)
	val {resDec=argDec1, resStr=argStr1, resExp=argExp1} =  
	    matchStr {sign=paramsig, str=argStr, strExp=argExp, entvar=entvar, 
		      entEnv=fctEntEnv, rpath=IP.IPATH[] (* ?DAVE *), 
		      statenv=statenv, region=region, compInfo=compInfo}

	(*** step #2: do the functor application ***)
	val argRlzn = case argStr1 of M.STR { rlzn, ... } => rlzn
				    | _ => M.bogusStrEntity 
	val bodyRlzn = EvalEntity.evalApp(fctRlzn, argRlzn, epc, rpath, compInfo)

	val resStr = 
	    let val bodyDacc = DA.namedAcc(anonSym,mkv)
	     in M.STR {sign=bodysig, rlzn=bodyRlzn,
		      access=bodyDacc, prim=[]}
	    end

	val resDec = 
	    let 
		val body = A.APPstr{oper=fct, arg=argStr1}
		val resAbs = A.LETstr(argDec1, body)
	     in A.STRdec [A.STRB{name=anonSym, str=resStr, def=resAbs}]
	    end

	val resExp = M.APPLY(fctExp, argExp1)
	val _ = debugmsg "<<applyFct"

     in {resDec=resDec, resStr=resStr, resExp=resExp}
    end
  | applyFct {fct=ERRORfct, ...} = 
      {resDec=A.STRdec [], resStr=M.ERRORstr, 
       resExp=M.CONSTstr M.bogusStrEntity}
  | applyFct _ = bug "applyFct:bad functor"

(*** top leve wrappers: used for profiling the compilation time *)
(*
val matchStr = 
  Stats.doPhase (Stats.makePhase "Compiler 034 1-matchStr") matchStr

val matchFct = 
  Stats.doPhase (Stats.makePhase "Compiler 034 2-matchFct") matchFct

val packStr = 
  Stats.doPhase (Stats.makePhase "Compiler 034 3-packStr") packStr

val applyFct = 
  Stats.doPhase (Stats.makePhase "Compiler 034 4-applyFct") applyFct
*)

end (* local *)
end (* structure SigMatch *)
