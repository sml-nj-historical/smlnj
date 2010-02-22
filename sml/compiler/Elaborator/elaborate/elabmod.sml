(* Copyright 1996 by AT&T Bell Laboratories *)
(* elabmod.sml *)

signature ELABMOD =
sig

  (* elaborate module-level declarations *)
  val elabDecl : 
         {ast       : Ast.dec,
          statenv   : StaticEnv.staticEnv,
          entEnv    : Modules.entityEnv,
          context   : ElabUtil.context,         (* elab context *)
          level     : bool,                     (* T if top-level decl. *)
          epContext : EntPathContext.context,
          path      : InvPath.path,
          region    : SourceMap.region,
          compInfo  : ElabUtil.compInfo} -> {absyn     : Absyn.dec,
                                             statenv   : StaticEnv.staticEnv} 
 
  val debugging : bool ref

end (* signature ELABMOD *)


structure ElabMod : ELABMOD =
struct

local structure SM = SigMatch
      structure S  = Symbol
      structure IP = InvPath
      structure SP = SymPath
      structure EP = EntPath
      structure EPC = EntPathContext
      structure EE = EntityEnv
      structure T  = Types
      structure TU = TypesUtil
      structure V  = VarCon
      structure M  = Modules
      structure MU = ModuleUtil
      structure MI = ModuleId
      structure L = Lookup
      structure EU = ElabUtil
      structure ET = ElabType
      structure EC = ElabCore
      structure ES = ElabSig
      structure B  = Bindings
      structure LU = Lookup
      structure INS = Instantiate
      structure SE = StaticEnv
      structure EM = ErrorMsg
      structure PP = PrettyPrint
      structure A  = Absyn
      structure DA = Access
      structure PPU = PPUtil
      structure ED = ElabDebug
      open Ast Modules
      open SpecialSymbols (* special symbols *)      
in

(* debugging *)
val say = Control_Print.say
val debugging = (* ElabControl.emdebugging *) ref true

fun debugmsg (msg: string) =
      if !debugging then (say msg; say "\n") else ()

fun bug msg = ErrorMsg.impossible("ElabMod: "^msg)

val debugPrint = (fn x => ED.debugPrint debugging x)

fun showStr(msg,str,env) =
    ED.withInternals(fn () =>
      debugPrint(msg,
		 (fn pps => fn str =>
		   PPModules.ppStructure pps (str, env, 100)),
		 str))

fun showFct(msg,fct,env) =
    ED.withInternals(fn () =>
      debugPrint(msg,
		 (fn pps => fn fct' =>
		   PPModules.ppFunctor pps (fct', env, 100)),
		 fct))

(* removeDuplicates : symbol list -> symbol list
   removes duplicate symbols from a list of symbols.
   Used on list of declared names passed to extractSig.
 *)
structure ST = RedBlackSetFn(type ord_key = S.symbol
                             val compare = S.compare) 

fun removeDuplicates syms =
    let fun helper([], memset, result) = rev result
	  | helper(s::rest, memset, result) = 
	    if ST.member(memset,s)
	    then helper(rest, memset, result)
	    else helper(rest, ST.add(memset,s), s::result)
    in helper(syms, ST.empty, [])
    end

(*
 * Check if an entity declaration is empty in order to avoid the unnecessary
 * recompilation bug reported by Matthias Blume (ZHONG)
 *)
fun nonEmptyEntDec (M.EMPTYdec | M.SEQdec []) = false
  | nonEmptyEntDec _ = true

fun seqEntDec ds = 
  let val nds = List.filter nonEmptyEntDec ds
   in case nds of [] => M.EMPTYdec
                | _ => M.SEQdec nds
  end

fun localEntDec(d1, d2) = seqEntDec [d1, d2] 

fun stripMarkSigb(MarkSigb(sigb',region'),region) =
      stripMarkSigb(sigb',region')
  | stripMarkSigb x = x

fun stripMarkFsigb(MarkFsigb(fsigb',region'),region) =
      stripMarkFsigb(fsigb',region')
  | stripMarkFsigb x = x

fun stripMarkFctb(MarkFctb(fctb',region'),region) =
      stripMarkFctb(fctb',region')
  | stripMarkFctb x = x

fun stripMarkStrb(MarkStrb(strb',region'),region) =
      stripMarkStrb(strb',region')
  | stripMarkStrb x = x

(* change of context on entering a structure *)
fun inStr (EU.TOP) = EU.INSTR
  | inStr z = z 

(* mapPaths moved to ModuleUtil. (DBM, 5/16/09) *)

(* 
 * ASSERT: order of DEFtycs in wtycs respects dependencies, i.e. no
 *         DEFtyc refers to tycons occurring after itself.
 * returns entityEnv and entityDec for type declarations. entityEnv is incremental
 *)
fun bindNewTycs(EU.INFCT _, epctxt, mkStamp, dtycs, wtycs, rpath, err)
      : EE.entityEnv * M.entityDec = 
    let fun stripPath path =
	    let val namePath = IP.IPATH[IP.last path]
	        val prefix = IP.lastPrefix path
	        val _ = if IP.equal(rpath,prefix) then ()
		        else err EM.WARN
			     "Harmless compiler bug: bad type path prefix"
			     EM.nullErrorBody
	     in namePath
	    end

	val vizty = (fn ty => #1(MU.relativizeType epctxt ty))
	val viztc = (fn tc => #1(MU.relativizeTyc epctxt tc))
           (* this is ok because epContext has state; a bit ugly *)
        val ndtycs =
            (case dtycs
	      of (T.GENtyc { kind, ... } :: _) =>
		 (case kind
		   of T.DATATYPE{index=0,family,freetycs, stamps, root} =>
                      let val rootev = mkStamp()
			  val rtevOp = SOME rootev
			  val nfreetycs = map viztc freetycs
			  val nstamps = Vector.map (fn _ => mkStamp()) stamps

			  fun newdt (dt as T.GENtyc {kind,arity,eq,path,...})=
			      (case kind
				 of T.DATATYPE{index=i,...} =>
				    let val (ev, rtev) = 
					    if i=0 then (rootev, NONE)
					    else (mkStamp(), rtevOp)

					val nkind = 
					    T.DATATYPE{index=i, stamps=nstamps,
						       freetycs=nfreetycs,
						       root=rtev,
						       family=family}
					(* the rtev field in DATATYPE indicates
					 * how to discover the new stamps when 
					 * such datatypes get evalent-ed *)

					val ndt =
					    T.GENtyc{arity=arity, eq=eq,
						     kind=nkind,
						     path=stripPath path, 
						     stamp=
						       Vector.sub(nstamps,i),
						     stub=NONE}

					val _ = 
					    EPC.bindTycEntVar(epctxt,
							    MU.tycId dt, ev)
				     in (ev, dt, M.FORMtyc ndt)
				    end
				 | _ => bug "unexpected case in newdtyc (1)")
                            | newdt _ = bug "unexpected case in newdtyc (2)"
                       in map newdt dtycs
                      end
		    | _ => bug "unexpected tycs in bindNewTycs (1)")
	       | [] => []
	       | _ => bug "unexpected tycs in bindNewTycs (2)")

        val nwtycs = 
            let fun newtc (tc as T.DEFtyc{stamp, tyfun=T.TYFUN{arity,body}, 
                                          strict, path}) =
                     let val ev = mkStamp()
                         val _ = EPC.bindTycEntVar(epctxt, MU.tycId tc, ev)
			 val ntc = 
                           T.DEFtyc{stamp=mkStamp(), strict=strict, 
				    path=stripPath path,
                                    tyfun=T.TYFUN{arity=arity, 
                                                  body=vizty body}}
                      in (ev, tc, M.FORMtyc ntc)
                     end
                  | newtc _ = bug "unexpected case in newwtyc"
             in map newtc wtycs
            end

        fun bind((ev,tc,te)::tcs, entEnv, entDecs) =
            bind(tcs, EE.bind(ev,M.TYCent(tc),entEnv),
                 M.TYCdec(ev, te)::entDecs)
          | bind(nil, entEnv, entDecs) =
            (EE.mark(mkStamp,entEnv), seqEntDec(rev entDecs))

     in bind(ndtycs@nwtycs, EE.empty, [])
    end

  | bindNewTycs _ = (EE.empty, M.EMPTYdec)


(***************************************************************************
 * extractSig : 
 *    SE.staticEnv *     -- the environment being translated info a full signature
 *    epContext *        -- epContext including entities in staticEnv
 *    context *          -- tells us if we are in a functor context
 *    symbol list *      -- gives declaration order of symbols in staticEnv
 *    compInfo           -- for mkStamp, to generate fresh entvars
 * -> M.elements *       -- elements of extracted signature (including slot numbers)
 *    M.entityEnv *      -- realization for full signature
 *    M.entityDec list * -- "redeclaration" of static entities
 *    B.binding list *   -- for original dyn. access of dynamic elements
 *    bool               -- flag to indicate presence of a functor (at any depth)

 * extractSig infers a signature and realization (entenv) for an arbitrary static
 * environment.
 * 
 * Recompute dynamic accesses as slots after the elaboration of a structure body,
 * replacing the original dynamic access by a SLOT and generating a
 * thinning (bindings) that will be used (in translate) to create the structure
 * record.
 * 
 * Also produces signature elements and matching entityEnv realization, and
 * corresponding new entityDecls.
 * 
 ***************************************************************************)
fun extractSig (env, epContext, context, declaredSymbols,
                compInfo as {mkStamp,...} : EU.compInfo) =
  let fun getEpOp (lookfn, modId) = lookfn (epContext, modId)
(*        case context
          of EU.INFCT _ => lookfn (epContext, modId)
           | _ => NONE
* INVARIANT(?): context /= INFCT => epContext = Empty, so lookfn would
  return NONE anyway if called in a nonfunctor context.
*)
      val relativize =
        case context
	  of EU.INFCT _ => (fn ty => #1(MU.relativizeType epContext ty))
	   | _ => fn x => x

      fun transBind ((sym, binding), 
                     (elements, entEnv, entDecls, bindings, slotCount, fctflag)) = 
        case binding
         of B.VALbind(V.VALvar{typ,path,...}) =>
              let val spec = VALspec{spec=relativize(!typ),
                                     slot=slotCount}
                  val elements' = (sym, spec) :: elements
               in (elements', entEnv, entDecls, binding::bindings, 
                   slotCount+1, fctflag)
              end

          | B.CONbind(dcon as T.DATACON{name,const,lazyp,sign,typ,rep}) =>
              let val typ' = relativize typ
                  val (rep', bindings', slotOp, slotCount') =
                    case rep
                     of DA.EXN _ => 
                          (DA.EXN(DA.nullAcc), binding::bindings,
                           SOME slotCount, slotCount+1)
                      | _ => (rep, bindings, NONE, slotCount)

                  val ndcon = T.DATACON{name=name, const=const, sign=sign,
                                        typ=typ', rep=rep', lazyp=lazyp}

                  val spec = CONspec{spec=ndcon, slot=slotOp}
                  val elements' = (sym, spec) :: elements

               in (elements', entEnv, entDecls, bindings', slotCount', fctflag)
              end

          | B.STRbind(str as STR{sign, rlzn, ...}) =>
	    let val epOp = getEpOp (EPC.lookStrEntPath, MU.strId str)  
                val (entVar, entEnv', entDecls') =
                    case epOp
                     of SOME [ev] => (ev, entEnv, entDecls)
                      | _ => 
                          let val ev = mkStamp()   (* fresh entvar *)
                              val ee = EE.bind(x, STRent rlzn, entEnv)
                              val ed =
                                case context
                                 of EU.INFCT _ => 
                                      let val strExp = 
                                              case epOp 
                                               of SOME ep => M.VARstr ep
						| _ => M.CONSTstr rlzn
                                       in (M.STRdec(ev, strExp, sym))::entDecls
                                      end
                                  | _ => entDecls
                           in (ev, ee, ed)
                          end

                  val spec = STRspec{sign=sign, slot=slotCount, def = NONE,
				     entVar=entVar}
                  val elements' = (sym, spec) :: elements
                  val fctflag' = 
                    (case sign 
                      of SIG sg => fctflag orelse #fctflag sg
                       | _ => fctflag)
               in (elements', entEnv', entDecls', binding::bindings,
                   slotCount+1, fctflag')
              end

          | B.FCTbind(fct as FCT {sign, rlzn, ...}) =>
	    let val epOp = getEpOp(EPC.lookFctEntPath, MU.fctId fct)
                val (entVar, entEnv', entDecls') =
                    case epOp
                     of SOME [ev] => (ev, entEnv, entDecls)
                      | _ => 
                         (let val ev = mkStamp()  (* fresh entvar *)
                              val ee = EE.bind(ev, FCTent rlzn, entEnv)
                              val ed =
                                case context
                                 of EU.INFCT _ => 
                                      (let val fctExp = 
                                             case epOp 
                                              of SOME ep => M.VARfct ep
                                               | _ => M.CONSTfct rlzn
                                        in (M.FCTdec(x, fctExp))::entDecls
                                       end)
                                  | _ => entDecl
                           in (ev, ee, ed)
                          end)

                  val spec = FCTspec{sign=sign,slot=slotCount,entVar=entVar}
                  val elements' = (sym, spec) :: elements
               in (elements', entEnv', entDecls', binding::bindings, 
                   slotCount+1, true)
              end

          | B.TYCbind tyc =>
              let val epOp = 
		      case tyc
			of T.ERRORtyc => NONE
			 | _ => getEpOp(EPC.lookTycEntPath, MU.tycId tyc)
                  val (entVar, entEnv', entDecls') =
                    case epOp
                     of SOME [x] => (x, entEnv, entDecls)
                        (* normal (i.e. locally declared) volatile case *)
                      | _ =>  (* NONE for nonvolatile declaration (not in functor) ,
                               * nonsingleton path for binding from opened param structure
			       * (Note [extractSig.1] *)
                          let val ev = mkStamp()  (* fresh entvar *)
                              val ee = EE.bind(ev, TYCent tyc, entEnv)
                              val ed =
                                case context
                                 of EU.INFCT _ => 
                                      (let val tycExp = 
                                             case epOp 
                                              of SOME ep => M.VARtyc ep
                                               | NONE => M.CONSTtyc tyc
                                        in (M.TYCdec(ev, tycExp))::entDecls
                                       end)
                                  | _ => entDecl
                           in (ev, ee, ed)
                          end

                  val spec = TYCspec{entVar=entVar,
                                     info=InfTycSpec{name=sym,arity=TU.tyconArity tyc}}
                  val elements' = (sym, spec) :: elements
                  (* 
                   * Use of T.ERRORtyc here is a hack. It relies on the
                   * fact that the inferred signature will never be 
                   * instantiated or signature-matched against. One might
                   * wonder what about a functor declaration with no result
                   * signature constraint ? the inferred fctSig would contain
                   * T.ERRORtyc --- fortunately the result-sig in this fctSig
                   * would never be matched against either. (ZHONG)
                   *)

               in (elements', entEnv', entDecls', bindings, slotCount, fctflag)
              end

          | _ => (elements, entEnv, entDecls, bindings, slotCount, fctflag)
        (* end transBind *)

	(* [GK 4/15/07] Consolidate will compact the potentially
	   linear static environment (i.e., BIND(...BIND(...)))
           into a hashtable (IntStrMap) and therefore eliminate
	   any connection between statenv binding order and the
	   structure declaration order. We use getDeclOrder to 
           extract the structure decl order and then use 
           SE.foldOverElems to compute the elements (specs) in 
           the structure decl order on the consolidated list. *)
        val cenv = SE.consolidate env 

        val (elements, entEnv, entDecls, bindings, _, fctflag) = 
          SE.foldOverElems(transBind, (nil, EE.empty, nil, nil, 0, false), cenv,
			   declaredSymbols)
	  handle SE.Unbound => bug "elabmod: extractSig -- SE.foldOverElems \
				    \Unbound symbol in origdeclorder"
     in (rev elements, entEnv, rev entDecls, rev bindings, fctflag)
    end (* fun extractSig *)


(****************************************************************************
 * constrStr : bool * strDec * Signature * Structure * entVar * ...
 * The constrStr function is used to carry out the signature matching
 * on structure declarations with signature constraints.
 * The first argument "transp" is a boolean flag; it is used to indicate
 * whether the signature matching should be done transparently (true) or
 * opaquely (false).
 * strDec is a dummy (e.g. <tempStr>) declaration form of the the structure
 * being matched.  entVar is the binding entVar for strDec, and it is 
 * passed to sigMatch.
 * The rest of the arguments are passed to sigMatch.
 ****************************************************************************)
fun constrStr(transp, strDec, sign, str, strExp, entvar, entEnv, rpath, 
              statenv, region, compInfo) : A.dec * M.Structure * M.strExp = 
  let val {resDec=resDec1, resStr=resStr1, resExp=resExp1} = 
          SM.matchStr{sign=sign, str=str, strExp=strExp, entvar=entvar, 
		      entEnv=entEnv, rpath=rpath, statenv=statenv, 
		      region=region, compInfo=compInfo}

   in if transp then (A.SEQdec[strDec, resDec1], resStr1, resExp1)
      else let val {resDec=resDec2, resStr=resStr2, resExp=resExp2} = 
                   SM.packStr{sign=sign, str=resStr1, strExp=resExp1, 
                              entEnv=entEnv, rpath=rpath, 
                              statenv=statenv, region=region, compInfo=compInfo}
           in (A.SEQdec[strDec, resDec1, resDec2], resStr2, resExp2)
           end
  end


(*** elabStr: elaborate the raw structure, without signature constraint ***)
(*** several invariants: 
      Every structure expression strexp is elaborated into a quadruple
      (absdec, str, exp, deltaEntEnv) where absdec is the corresponding abstract
      syntax tree, str is the resulting (static) structure, exp is the entity 
      expression, and deltaEntEnv is the delta entity environment collected while
      elaborating the current structure expression. The deltaEntEnv is
      designed to deal with LetStr and LetFct and to "maintain the hidden
      entity environment context" (clarify?).
 *)
fun elabStr
      (strexp: Ast.strexp,         
       name: S.symbol option, 
       env: SE.staticEnv,
       entEnv: M.entityEnv,
       context: EU.context,
       epContext: EPC.context,   
       entVarOp: EP.entVar option,  (* entVar for _this_ structure (when is it SOME?) *)
       rpath: IP.path,
       region: SourceMap.region,      
       compInfo as {mkLvar=mkv, mkStamp, error, ...}: EU.compInfo)
      : A.dec * M.Structure * M.strExp * EE.entityEnv =
let
 
(* name will be SOME n if the strexp is the definiens in a structure declaration.
 * it will be NONE if strexp is an anonymous str expression *)
val sname =  case name of SOME n => S.name n
                        | NONE => "<anonymous>"

val _ = debugmsg (">>elabStr: " ^ sname)
             
(* 
 * elab: Ast.strexp * staticEnv * entityEnv * region
 *       -> A.dec * M.Structure * M.strExp * EE.entityEnv
 *)
fun elab (BaseStr decl, env, entEnv, region) =
      let val _ = debugmsg ">>elab[BaseStr]"

          (* we enter the epcontext when we get into BaseStr *)
          val epContext'= case entVarOp
			   of NONE => epContext
			    | SOME entVar => EPC.enterOpen(epContext,entVar) 

	  (* elaborating the body declarations *)
          val (absDecl, entDecl, env', entEnv') =  (* entEnv' is a delta env *)
                 elabDecl0(decl, env, entEnv, inStr context, true,
                           epContext', rpath, region, compInfo)
          val _ = debugmsg "--elab[BaseStr]: elabDecl0 done"
   
	  (* We use the declaredSymbol list to order the elements in the extracted/inferred
           * signature to ensure more accurate printing of signatures. *)
	   [GK 4/18/07] *)
          val delcaredSymbols = suppressDuplicates(AbsynUtil.declaredSymbols absDecl)

          (* extracting a signature from the body environment *)
          val (elements, entEnv'', entDecls, locations, fctflag) =
                extractSig(env', epContext', context, declaredSymbols, compInfo)
          val _ = debugmsg "--elab[BaseStr]: extractSig done"

          val (entEnvLocal, entDecLocal) =
              case context
               of EU.INFCT _ => 
                    (EE.mark(mkStamp,EE.atop(entEnv'',entEnv')), (* why only in a functor? *)
                     seqEntDec(entDecl::entDecls))
		| _ => (entEnv'', entDecl)

          val strExp = STRUCTURE{stamp=M.NEW, entDec=entDecLocal}

          val resStr = 
            let val sign = (* the inferred signature *)
                  M.SIG{stamp = mkStamp (),
			name=NONE, closed=false, fctflag=fctflag,
                        elements=elements, 
			properties = PropList.newHolder (),
                        typsharing=nil, strsharing=nil,
			stub=NONE}
 
                val strRlzn =
                    {stamp = mkStamp(), (* generate structure stamp *)
		     entities=EE.mark(mkStamp,EE.atop(entEnvLocal, entEnv)),
		     properties = PropList.newHolder (),
		     rpath=rpath, stub = NONE}

                val dacc = DA.namedAcc(tempStrId, mkv)

                val prim = MU.strPrimElemInBinds locations

            in M.STR {sign=sign, rlzn=strRlzn, access=dacc, prim=prim}
            end

          val _ = debugPrint("BaseStr after resStr  - symbols: ", ED.ppSymList,
                             ED.envSymbols env')

          val resDec = 
            let val body = A.LETstr(absDecl, A.STRstr(locations))
             in A.STRdec [A.STRB {name=tempStrId, str=resStr, def=body}]
            end

          val _ = debugmsg "<<elab[BaseStr]"

       in (resDec, resStr, strExp, EE.empty)
      end

  | elab (AppStr(spath,args), env, entEnv, region) =
      let val strexp' = LetStr(StrDec[Strb{name=returnId,constraint=NoSig,
					   def=AppStrI(spath,args)}],
			       VarStr([returnId,resultId]))
       in elab(strexp', env, entEnv, region)
      end

  | elab (AppStrI(spath,[(arg,b)]), env, entEnv, region) =
      let val _ = debugmsg ">>elab[AppStr-one]"

          (* lookup functor designated by spath *)
          val fct = LU.lookFct(env, SP.SPATH spath, error region)

          val _ = debugmsg "--elab[AppStr-one]: functor lookup done"
          val _ = showFct("--elab[AppStr]: functor ",fct,env)

          val entv = mkStamp()   (* fresh entVar for the uncoerced argument *)
          val (argDec, argStr, argExp, argDee) = 
	      elabStr(arg, NONE, env, entEnv, context, epContext,
		      SOME entv, IP.IPATH[], region, compInfo)

          val _ = debugmsg "--elab[AppStr-one]: elab arg done"
          val _ = showStr("--elab[AppStr-one]: arg str: ",argStr,env)

       in case (fct,argStr)
	    of ((M.ERRORfct,_) | (_,M.ERRORstr)) =>
		(debugmsg "<<elab[AppStr-one]: error fct or arg";
		 (A.SEQdec[], M.ERRORstr, M.CONSTstr(M.bogusStrEntity), EE.empty))
	     | (M.FCT { rlzn = fctEnt, ... }, M.STR { rlzn = argEnt, ... }) =>
	       let val resDee =
		       EE.mark(mkStamp, EE.bind(entv, M.STRent argEnt, argDee))
		        (* the argument structure should be bound to entv *)
		   (* val _ = debugPrint("--elab[AppStr]: epContext = ",
				   fn ppstrm => 
			       fn ctx => 
				  PPModules.ppEPC ppstrm (ctx, 100),
			     epContext) 
                    *)

		   (* is the functor volatile or nonvolatile? *)
		   val fctExp = 
			case EPC.lookFctEntPath(epContext, MU.fctId fct)
			  of SOME ep => VARfct ep    (* volatile, use variable ref *)
			   | NONE => CONSTfct fctEnt (* nonvolatile, a constant *)

		   val epc = case entVarOp (* from outer evalStr call *)
			       of NONE => epContext
				| SOME ev => EPC.enterOpen(epContext, ev)
		   (* val _ = debugPrint("--elab[AppStr]: epc = ",
				   fn ppstrm => 
			       fn ctx => 
				  PPModules.ppEPC ppstrm (ctx, 100),
			     epc)
                    *)

		   val {resDec, resStr, resExp} = 
			SM.applyFct{fct=fct, fctExp=fctExp, argStr=argStr, 
				    argExp=argExp, entvar = entv, epc=epc,
				    statenv=env, rpath=rpath, region=region,
				    compInfo=compInfo}
		   val _ = debugmsg "--elab[AppStr-one]: applyFct done"
		   val _ = showStr("--elab[AppStr-one]: result: ",resStr,env)
		   val _ = debugmsg "<<elab[AppStr-one]"
		in (A.SEQdec [argDec, resDec], resStr, resExp, resDee)
	       end
	     | _ => bug "AppStrI:one arg"
      end (* AppStrI - one arg *)

  | elab (AppStrI(spath,arg :: larg), env, entEnv, region) =
      let val _ = debugmsg ">>elab:[AppStr-many]"
          val strexp' = LetStr(StrDec[Strb{name=hiddenId,constraint=NoSig,
                                           def=AppStrI(spath,[arg])}],
                               AppStrI([hiddenId,functorId],larg))
       in elab(strexp', env, entEnv, region)
      end (* AppStrI - multiple args *)

  | elab (AppStrI(spath,[]), env, entEnv, region) =
      bug "elabStr.AppStrI -- empty arg list"

  | elab (v as VarStr path, env, entEnv, region) =
      let val _ = debugmsg ">>elab[VarStr]"
          val str = LU.lookStr(env,SP.SPATH path,error region)
(*
          val _ = showStr("--elab[VarStr]: str: ",str,env)
*)
          val strRlzn = 
		case str
		  of STR { rlzn, ... } => rlzn
		   | _ => M.bogusStrEntity  (* error recovery *)
          val resExp =
	        case str
		  of STR _ =>
		      ((* debugmsg "--elab[VarStr]: resExp/STR"; *)
		       case EPC.lookStrEntPath(epContext,MU.strId str) 
			 of NONE => M.CONSTstr strRlzn
			  | SOME ep => M.VARstr ep)
		   | _ => M.CONSTstr M.bogusStrEntity (* error recovery *)

       in debugmsg "<<elab[VarStr]"; (* GK: Used to be commented out *) 
	  (A.SEQdec [], str, resExp, EE.empty)
      end

  | elab (LetStr(decl,str), env, entEnv, region) =
      let val _ = debugmsg ">>elab[LetStr]"
          val (localAbsDec, localEntDecl, env', entEnv') = 
            elabDecl0(decl, env, entEnv, context, true,
                      epContext, rpath, region, compInfo)
	     (* top = true: don't allow nongeneralized type variables
              * in local decls because of bug 905/952.  This is
	      * stricter than necessary.  Could allow top = false
	      * if the body str contains no functors.  To make the
              * condition more precise, have to synthesize a boolean
	      * attribute indicating presence of functors [dbm] *)
                (* 
                 * DAVE? what context to use for the local decls?
                 * perhaps should null bindContext as for functor body?
                 * perhaps it doesn't matter because of relativization
                 * and the fact that local entities can't be referred
                 * to from outside. 
                 *)
          val _ = debugmsg "--elab[LetStr]: local elabDecl0 done"
          val (bodyAbsDec, bodyStr, bodyExp, bodyDee) = 
            elab(str, SE.atop(env',env), EE.atop(entEnv',entEnv), region)

          val resDec = A.SEQdec [localAbsDec, bodyAbsDec]
          val resExp = M.LETstr (localEntDecl, bodyExp)
          val _ = debugmsg "<<elab[LetStr]: elab body str done"

       in (resDec, bodyStr, resExp, EE.mark(mkStamp,EE.atopSp(bodyDee,entEnv')))
      end

  | elab (ConstrainedStr(strexp,constraint), env, entEnv, region) =
      let val (csigexp,transp) =
	      case constraint
	       of NoSig => bug "elabStr: ConstrainedStr with no sig"
		| Transparent sigexp => (sigexp, true)
		| Opaque sigexp => (sigexp, false)

	  val csig =
              ES.elabSig {sigexp=csigexp, nameOp=NONE, env=env, 
			  entEnv=entEnv, epContext=epContext,
			  region=region, compInfo=compInfo}

          val entsv = mkStamp()

          (** elaborating the structure body *)
          val (strAbsDec, str, exp, deltaEntEnv) = 
              elabStr(strexp, NONE, env, entEnv, context,
                      epContext, SOME entsv, rpath, region, compInfo)

          val resDee = 
              let val strEnt =
                      case str of M.STR { rlzn, ... } => rlzn
                                | _ => M.bogusStrEntity
              in (EE.bind(entsv, M.STRent strEnt, deltaEntEnv))
              end

          (** elaborating the signature matching *)
          val (resDec, resStr, resExp) = 
              constrStr(transp, strAbsDec, csig, str, exp, entsv,
                        entEnv, rpath, env, region, compInfo)

       in (resDec, resStr, resExp, resDee)
      end

  | elab (MarkStr(strexp',region'),env,entEnv,region) = 
      let val (resDec, str, resExp, resDee) = 
            elab(strexp', env, entEnv, region')
       in (A.MARKdec(resDec, region'), str, resExp, resDee)
      end

val (resDec, resStr, resExp, resDee) = elab(strexp, env, entEnv, region)
val _ = debugmsg "<<elabStr"

in (resDec, resStr, resExp, resDee)
end (* function elabStr *)


(*** elabFct: elaborate the functor, possibly with signature constraint ***)
and elabFct
      (fctexp: Ast.fctexp, 
       curried : bool,
       name: S.symbol,   
       env: SE.staticEnv,
       entEnv: M.entityEnv,
       context: EU.context,
       epContext: EPC.context,
       rpath: IP.path,
       region: SourceMap.region,
       compInfo as {mkLvar=mkv, mkStamp, error, ...}: EU.compInfo)
      : A.dec * M.Functor * M.fctExp * EE.entityEnv =

let

val _ = debugmsg (">>elabFct: " ^ (S.name name))

in

case fctexp
 of VarFct(spath,constraintExpOp) =>
      let val _ = debugmsg "--elabFct[VarFct]"
	  val fct = LU.lookFct(env,SP.SPATH spath,error region)
       in case fct
	    of ERRORfct =>
		(A.SEQdec [], fct, CONSTfct(M.bogusFctEntity), EE.empty)
	     | _ =>
		let val uncoercedExp = 
		        (case EPC.lookFctEntPath(epContext, MU.fctId fct)
			   of SOME ep => (debugmsg "--elabFct[VarFct] VARfct";
					  VARfct ep)
			    | NONE =>
			       let val _ = debugmsg "--elabFct[VarFct] CONSTfct"
				   val rlzn = case fct of FCT ft => #rlzn ft
							| _ => M.bogusFctEntity
			        in CONSTfct rlzn
			       end)

		 in case constraintExpOp
		      of NoSig => (A.SEQdec [], fct, uncoercedExp, EE.empty)
		       | Transparent astFsig =>
			  let val nameOp = SOME(anonfsigId)
			      val fsig = 
				  ES.elabFctSig
				    {fsigexp=astFsig, nameOp=nameOp, env=env,
				     entEnv=entEnv, epContext=epContext, 
				     region=region, compInfo=compInfo}

			      val {resDec, resFct, resExp} =
				  SM.matchFct
				    {sign=fsig, fct=fct, fctExp=uncoercedExp,
				     entEnv=entEnv, 
				     rpath=rpath, statenv=env, region=region, 
				     compInfo=compInfo}
			   in (resDec, resFct, resExp, EE.empty)
			  end
		       | Opaque astFsig => 
                          bug "Opaque functor constraint not impl"
		end
      end

  | LetFct(decl,fct) =>
    (* fct is always either a functor name, or another LetFct *)
      let val _ = debugmsg ">>elab[LetFct]"
          val (localAbsDec, localEntDecl, env', entEnv') = 
            elabDecl0(decl, env, entEnv, context, true,
	              epContext, rpath, region, compInfo)
           (* top = true: don't allow nongeneralized type variables
              in local decls because of bug 905/952 [dbm] *)
          val _ = debugmsg "--elab[LetFct]: local elabDecl0 done"
          val (bodyAbsDec, bodyFct, bodyExp, bodyDee) = 
            elabFct(fct, false, name, SE.atop(env',env), EE.atop(entEnv',entEnv),
                    context, epContext, rpath, region, compInfo)

          val resAbsyn = A.SEQdec [localAbsDec, bodyAbsDec]
          val resExp = M.LETfct(localEntDecl, bodyExp)
          val resDee = EE.mark(mkStamp, EE.atopSp(bodyDee, entEnv'))

       in (resAbsyn, bodyFct, resExp, resDee)
      end

  | AppFct(spath,larg,constraint) =>
      let val _ = debugmsg "--elabFct[AppFct]"
	  val fctexp' = LetFct(StrDec[Strb{name=hiddenId,constraint=NoSig,
                                           def=AppStrI(spath,larg)}],
                               VarFct([hiddenId,functorId],constraint))

       in elabFct(fctexp', false, name, env, entEnv, context, epContext,
                  rpath, region, compInfo)
      end

  (* uncurried functor *)
  | BaseFct{params=[(paramNameOp,paramSigExp)],body,constraint} =>
      let val _ = debugmsg ">>elabFct[BaseFct]"
	  val body = if curried then body
	               (* body has already been transformed in curried case
			  of elabFct below, and is a BaseStr containing a
			  single functor declaration *)
		     else BaseStr(StrDec[Strb{name=resultId, def=body,
					      constraint=constraint}])
	  val constraint = if curried then constraint else NoSig
              (* if curried=true, then constraint = NoSig !!!
               * -- the result signature (if any) will always be delegated
	       * to a declaration (StrDec or FctDec) within a body wrapper. *)

          val flex =
	      case context
	       of EU.INFCT {flex} => flex
                   (* already in body of a parent functor -- we don't differentiate
		    * flex tycons according to depth of functor abstractions *)
		| _ => (*** Entering functor for first time ***) 
		  (* any stamps generated "hereafter" considered flexible *)
		   let val base = mkStamp() 
		    in (fn s => (case Stamps.compare(base,s)
				   of LESS => true
				    | _ => false))
		   end

          val paramName = case paramNameOp of NONE => paramId
                                            | SOME n => n

          val paramEntVar = mkStamp()

	  val _ = debugmsg (">>elabFct[BaseFct]: paramEntVar = "^
			    EP.entVarToString paramEntVar)
	  (* val _ = debugPrint("--elabFct[BaseFct]: epContext = ",
			     fn ppstrm => 
			       fn ctx => 
				  PPModules.ppEPC ppstrm (ctx, 100),
			     epContext) 
           *)
		     
          val paramSig = 
              ES.elabSig {sigexp=paramSigExp, nameOp=NONE, env=env, 
                          entEnv=entEnv, epContext=epContext, 
                          region=region, compInfo=compInfo}
          val _ = debugmsg "--elabFct[BaseFct]: paramSig defined"

	  val _ = case paramSig
		    of ERRORsig => raise EM.Error
		        (* bail out -- not attempting to recover from bad signature *)
		     | _ => ()

	  (* now we can assume that paramSig is defined *)
          (* we formally instantiate the paramSig to get a realization (paramRlzn).
           * This creates new stamps, but we don't bother to add them to the
           * epcontext while instantiating, we do it below using mapPaths.
	   * All fresh stamps created during this instantiation will be considered
	   * flexible/formal.
	   *)
          val {rlzn = paramRlzn, primaries=primaries} =
              INS.instFormal
                {sign=paramSig, entEnv=entEnv, region=region,
		 rpath=IP.IPATH(case paramNameOp
				 of NONE => []
				  | _ => [paramName]),
		 compInfo=compInfo}

          val paramStr = 
              M.STR{sign=paramSig, rlzn=paramRlzn, 
                    access=DA.namedAcc(paramName, mkv), prim=[]}

          val _ = debugmsg "--elabFct[BaseFct]: param instantiated"
          val _ = showStr("--elabFct[BaseFct]: paramStr: ",paramStr,env)

          (* bind parameter entity to paramEntVar in entity env *)
          val entEnv' = 
              EE.mark(mkStamp,EE.bind(paramEntVar,M.STRent paramRlzn,entEnv))
          val _ = debugmsg "--elabFct[BaseFct]: param EE.bind"

	  val _ = debugmsg "--elabFct[BaseFct]: elabmod before env'\n"
          val env' =
              case paramNameOp 
               of NONE => MU.openStructure(env,paramStr)
		| SOME _ => SE.bind(paramName, B.STRbind paramStr, env)
          val _ = debugmsg "--elabFct[BaseFct]: param bound/opened"

          (* push a functor abstraction layer onto epContext, with a new,
	   * empty pathMap and an empty context path. *)
          val epContext' = EPC.enterClosed epContext 

	  (* add parameEntVar to context of epContext for purpose of updating
	   * locals pathmap with elements of paramStr. The context path of
	   * epContextParam is [paramEntVar].
	   * Then extend the locals pathmap of the epcontextParam, which is the
	   * same as the locals of epContext', with paths for elements of
           * paramStr *)
          val _ = let val epContextParam = 
			  EPC.enterOpen(epContext', paramEntVar) 
		   in MU.mapPaths(epContextParam, paramSig, paramRlzn, flex)
		  end

          (* add mapping from the paramStr to the locals pathmap.  The strId
           * of paramStr will be mapped to the path [paramEntVar] since the
           * context path of epContext' is [] *)
          val _ = EPC.bindStrEntVar(epContext', MU.strId paramStr, paramEntVar)
          val _ = debugmsg "--elabFct[BaseFct]: epContext initialized"

          (* adjust the EU.context value. Note that context' = context if
	   * we were already inside a functor. *)
          val context' = EU.INFCT{flex=flex}

          (* bodyDee was discarded here; however, it was not discarded when
             functor is applied. *)
          val (bodyAbsDec, bodyStr, bodyExp, bodyDee) = 
              elabStr(body, NONE, env', entEnv', context', epContext', NONE,
                      IP.IPATH [], region, compInfo)
          val _ = debugmsg "--elabFct[BaseFct]: body elaborated"
          val _ = showStr("--elabFct[BaseFct]: bodyStr: ",bodyStr,env)

          val fctExp = M.LAMBDA{param=paramEntVar,
				body=bodyExp}

          val resFct = 
            let val (bodySig,bodyRlzn) = 
		    case bodyStr 
		     of STR { sign, rlzn, ... } => (sign, rlzn)
		      | _ => (ERRORsig, bogusStrEntity)

                val fctSig = 
                    M.FSIG{kind=NONE, paramsig=paramSig, 
                           bodysig=bodySig, paramvar=paramEntVar, 
                           paramsym=paramNameOp}

                val rlzn = {stamp = mkStamp(),
			    exp = LAMBDA{param=paramEntVar, 
					 body=bodyExp},
			    primaries=primaries,
			    closureEnv = entEnv, 
			      (* Closure: Using the old entity environment !! *)
			    paramEnv = #entities paramRlzn,
			      (* or just paramRlzn? Where and how used? *)
			    properties = PropList.newHolder (),
			    rpath = rpath,
			    stub = NONE}

                val dacc = DA.namedAcc(name, mkv)

             in M.FCT{sign=fctSig, rlzn=rlzn, access=dacc, prim=[]}
            end

          val _ = debugmsg "--elabFct[BaseFct]: resFct defined"

          val resDec =
            let val fctdef = A.FCTfct{param=paramStr,
                                      def=A.LETstr(bodyAbsDec,A.VARstr bodyStr)}
             in A.FCTdec [A.FCTB {name=name, fct=resFct, def=fctdef}]
            end

          val _ = debugmsg "<<elabFct[BaseFct]"
          val _ = showStr("--elabFct[BaseFct]: paramStr: ",paramStr,env)

       in (resDec, resFct, fctExp, EE.empty)
      end (* BaseFct - uncurried *)

  (* curried functor *)
  | BaseFct{params=param :: lparam,body,constraint} =>
      let val _ = debugmsg "--elabFct[BaseFct] curried"
	  val fctexp' = 
            BaseFct{params=[param],
		    body=BaseStr(
                           FctDec[Fctb{name=functorId,
                                       def=BaseFct{params=lparam, body=body,
                                                  constraint=constraint}}]),
		    constraint=NoSig}

       in elabFct(fctexp', true, name, env, entEnv, context, epContext,
                  rpath, region, compInfo)
      end

  | BaseFct{params=[],...} => bug "elabFct"

  | MarkFct(fctexp',region') =>
      elabFct(fctexp', curried, name, env, entEnv, context, epContext,
              rpath, region', compInfo)

end (* end of function elabFct *)


(*** elabStrbs: elaborate structure bindings, with signature constraint ***)
and elabStrbs(strbs: Ast.strb list, 
              env0: SE.staticEnv,
              entEnv0: M.entityEnv,
              context: EU.context,
              epContext: EPC.context,
              rpath: IP.path,
              region: SourceMap.region,
              compInfo as {mkStamp,mkLvar=mkv,error,...}: EU.compInfo) 
            : A.dec * M.entityDec * SE.staticEnv * entityEnv =
let

val _ = debugmsg ">>elabStrbs"

fun loop([], decls, entDecls, env, entEnv) = 
      let val _ = debugmsg "<<elabStrbs"
          val resDec = A.STRdec (rev decls)
          val entDec = case entDecls of [] => M.EMPTYdec
                                      | _ => seqEntDec(rev entDecls)
       in (resDec, entDec, env, entEnv)
      end

  | loop(strb::rest, decls, entDecls, env, entEnv) = 
      let val _ = debugmsg ">>elabStrbs"
          val (name, constraint, def, region') =
              case stripMarkStrb(strb,region)
                of (Strb{name=n,constraint=c,def=d},r) => (n, c, d, r)
                 | _ => bug "non structure bindings in elabStrbs"
          val _ = debugmsg("--elabStrbs: structure "^S.name name)

          (* entsv is the context for evaluating the right-handside 
             of a structure declaration *)
          val csigexpOp =
	      case constraint
	       of NoSig => NONE
		| Transparent sigexp => SOME(sigexp, true)
		| Opaque sigexp => SOME(sigexp, false)
	  val csigOp =
	      case csigexpOp
	       of NONE => NONE
	        | SOME (sigexp, transp) =>
		  let val csig = 
			  ES.elabSig {sigexp=sigexp, nameOp=NONE, env=env0, 
				      entEnv=entEnv0, epContext=epContext,
				      region=region, compInfo=compInfo}
		  in case csig
		      of ERRORsig => NONE
		       (* if constraint doesn't elaborate,
			* pretend it didn't exist *)
		       | _ => SOME(csig, transp)
		  end

          (* create an entity variable for the current str declaration *)
          val entv = mkStamp()   (* we don't always have to do this *)
          (* create a second entVar for the intermediate structure when
	   * there is a signature constraint *)
          val entsv = 
              case csigOp
               of NONE => entv  (* no constraint => entsv = entv *)
                | _ => mkStamp()

          (** elaborating the structure body.
           *  defAbsDec with be a binding of the actual structure to <tempStr> *)
          val (defAbsDec, defStr, defExp, deltaEntEnv) =
              elabStr(def, SOME name, env0, entEnv0, context, epContext,
                      SOME entsv, IP.extend(rpath,name), region', compInfo)

          val _ = debugmsg "--elabStrbs: elabStr done"
(*
	  val _ = showStr("unconstrained structure: ",defStr,env)
*)
          (** elaborating the signature matching: notice that we did
              introduce stamps during the abstraction matching, but
              these stamps are always visible, thus will always be
              caught by the post sig-matching "mapPaths" function call. *)
          val (resDec, resStr, resExp) = 
              case csigOp
	       of NONE => (defAbsDec, defStr, defExp)
		| SOME (csig,transp) => 
                    constrStr(transp, defAbsDec, csig, defStr, defExp, 
                              entsv, entEnv0, IP.IPATH[name], 
                              StaticEnv.atop(env,env0), region, compInfo)

          val deltaEntEnv = 
              case csigOp
               of NONE => deltaEntEnv
                | SOME _ =>
                    (case defStr
                      of M.STR { rlzn, ... } =>
                          EE.bind(entsv, M.STRent rlzn, deltaEntEnv)
                       | _ =>
                          EE.bind(entsv, M.STRent M.bogusStrEntity, deltaEntEnv))

          val _ = debugmsg "--elabStrbs: constrain done" 
           
          val _ = showStr("--elabStrbs: resStr: ",resStr,env)

          (* We create a second copy of resStr, named bindStr, that differs only
           * in that it has a different lvar as its access.  This is required because
           * the binding is double layered (see definition of bindAbs below),
	   * where bindStr is associated with the outer binding to name, while
	   * resStr is associated with the inner binding in resDec (e.g. to
	   * <tempStr>).
           * WARNING: bindStr modifies the access field of resStr; this
           * may create structures with same modIds but different dynamic
           * accesses --- BUT, we assume that before or during the pickling, 
           * both the dynamic access and the inl_info will be updated 
           * completely and replaced with proper persistent accesses (ZHONG)
           *)
          val (bindStr, strEnt) = 
            case resStr
             of STR { rlzn, sign, access, prim } =>
		let val access2 = DA.namedAcc(name, mkv)
		    val str = STR{rlzn = rlzn, sign = sign, access = access2,
				  prim = prim}
		in
		    (str, M.STRent rlzn)
		end
              | _ => (resStr, M.STRent M.bogusStrEntity)

          val _ = showStr("--elabStrbs: bindStr: ",bindStr,env)

          (* this is a two-layer binding, with the outer binding to name and
	   * an inner binding in resDec for the rhs strexp *)
          val bindAbs = A.STRB{name = name,
			       str = bindStr, 
                               def = A.LETstr(resDec, A.VARstr resStr)}
          val decls' = bindAbs :: decls

          val (entEnv', entDecls') = 
            case context
             of EU.INFCT {flex,...} => 
                   let val entEnv1 = EE.atopSp(deltaEntEnv, entEnv)
                       val entEnv2 = EE.bind(entv, strEnt, entEnv1)
                       val entEnv3 = EE.mark(mkStamp, entEnv2)

		       val _ = debugmsg "--elabStrbs: about to mapPaths bindStr"
                       (*
                        * We are remapping entPaths for elements of
                        * the new structure unconditionally, even if
                        * there is no signature constraint and the
                        * defining strexp is BaseStr (DAVE).
                        *)
		       val _ = case bindStr
				of STR{sign, rlzn, ...} =>
				   MU.mapPaths(EPC.enterOpen(epContext, entv),
					       sign, rlzn, flex)
				 | _ => ()
		       val _ = debugmsg "--elabStrbs: mapPaths bindStr done"
		       val _ = (case bindStr
				 of STR { sign, rlzn, ... } =>
				    EPC.bindStrEntVar(epContext, 
						      MU.strId2(sign,rlzn), entv)
				  | _ => ())
		  
                   in (entEnv3, ((M.STRdec(entv, resExp, name))::entDecls))
                   end
              | _ => (entEnv, entDecls)

          val _ = showStr("--elabStrbs: bindStr: ",bindStr,env)

          val env' = SE.bind(name, B.STRbind bindStr, env)

       in loop(rest, decls', entDecls', env', entEnv')
      end

 in loop(strbs, [], [], SE.empty, EE.empty)
      handle EE.Unbound => 
       (debugmsg("$elabStrbs0: StrDec");
        raise EE.Unbound)

end (* end of function elabStrbs *)


(*** elabDecl0: elaborate an arbitrary module-level declaration ***)
and elabDecl0 
      (decl: Ast.dec,
       env0: SE.staticEnv,
       entEnv0: M.entityEnv,
       context: EU.context,
       top: bool,
       epContext: EPC.context,
       rpath: IP.path,
       region: SourceMap.region,
       compInfo as {mkStamp,mkLvar=mkv,error,anyErrors,transform,...}
         : EU.compInfo)
      : A.dec * entityDec * SE.staticEnv * entityEnv =
      (* entityDec and entityEnv are for bindings in decl *)

(case decl
  of StrDec strbs =>
       elabStrbs(strbs, env0, entEnv0, context, epContext, 
                 rpath, region, compInfo)

   | OpenDec paths =>
       let val err = error region
           val strs = map (fn s => let val sp = SP.SPATH s
                                    in LU.lookStr(env0, sp, err)
                                   end) paths
           fun loop([], env) = (A.OPENdec strs, M.EMPTYdec, env, EE.empty)
             | loop(s::r, env) = loop(r, MU.openStructure(env, s))

        in loop(strs, SE.empty)
       end 

   | FctDec fctbs =>
       let val _ = debugmsg ">>elabFctbs"

           fun loop([], decls, entDecls, env, entEnv) = 
                 let val resDec = A.FCTdec (rev decls)
                     val entDec = case entDecls of [] => M.EMPTYdec
                                                 | _ => seqEntDec(rev entDecls)
                     val _ = debugmsg "<<elabFctbs"
                  in (resDec, entDec, env, entEnv)
                 end

             | loop(fctb::rest, decls, entDecls, env, entEnv) = 
                 let val (name, def, region') =
                       case stripMarkFctb(fctb,region) 
                        of (Fctb{name=n, def=d}, r) => (n, d, r)
                         | _ => bug "non functor bindings for FctDec fctbs"
                     val _ = debugmsg("--elabDecl0: functor "^S.name name)

                     (* dynamic access is already assigned in elabFct *)
                     val (fctAbsDec, fct, fctExp, deltaEntEnv) = 
                         elabFct(def, false, name, env0, entEnv0, context,
				 epContext, rpath, region', compInfo)
  
                     (*
                      * WARNING: bindFct modifies the access field of fct; 
                      * this may create functors with same modIds but 
                      * different dynamic accesses --- BUT, we assume that 
                      * before or during the pickling, both the dynamic 
                      * access and the inl_info will be updated completely 
                      * and replaced with proper persistent accesses (ZHONG)
                      *)
                     val (bindFct, fctEnt) = 
                       case fct
			 of FCT {rlzn, sign, access, prim} =>
			    (FCT{rlzn = rlzn, sign = sign, prim = prim,
                                 access = DA.namedAcc(name, mkv)},
			     FCTent rlzn)
			  | ERRORfct => (fct, ERRORent)

                     val fb = A.FCTB{name = name, fct = bindFct, 
                                     def = A.LETfct(fctAbsDec,A.VARfct fct)}
                     val decls' = fb :: decls

                     val (entEnv', entDecls') = 
                       case context
                        of EU.INFCT _ => 
                            (let val entVar = mkStamp()
                                 val _ = case bindFct
				           of FCT _ =>
					       EPC.bindFctEntVar(epContext, 
                                                 MU.fctId bindFct, entVar)
					    | ERRORfct => ()
                                 val entEnv1 = EE.atopSp(deltaEntEnv, entEnv)
                                 val entEnv2 = EE.bind(entVar, fctEnt, entEnv1)
                                 val entEnv3 = EE.mark(mkStamp, entEnv2)
                              in (entEnv3, M.FCTdec(entVar,fctExp)::entDecls)
                             end)
                         | _ => (entEnv, entDecls)

                     val env' = SE.bind(name, B.FCTbind bindFct, env)
  
                  in loop(rest, decls', entDecls', env', entEnv') 
                 end

        in loop(fctbs, nil, nil, SE.empty, EE.empty)
           handle EE.Unbound => (debugmsg("$elabDecl0: FctDec"); 
                                raise EE.Unbound)
       end (* FctDec fctbs *)                            

   | SigDec sigbs =>
       let val _ = debugmsg ">>elabSigbs"

           fun loop([], sigs, env) = 
                 let val _ = debugmsg "<<elabSigbs"
                  in (A.SIGdec (rev sigs), M.EMPTYdec, env, EE.empty)
                 end

             | loop (sigb::rest, sigs, env) =
                 let val (name, def, region') =
                       case stripMarkSigb(sigb,region)
                        of (Sigb{name=n,def=d},r) => (n, d, r)
                         | _ => bug "non signature bindings for SigDec sigbs"
                     val _ = debugmsg("--elabDecl0: signature "^S.name name)

                     val s = 
                       ES.elabSig {sigexp=def, nameOp=SOME name, env=env0, 
                                   entEnv=entEnv0, epContext=epContext, 
                                   region=region', compInfo=compInfo}
		     val _ = (* instantiate to check well-formedness, 
                              * but only if there have been no earlier errors *)
			if !ElabControl.instantiateSigs
                           andalso not(!(#anyErrors compInfo))
			then (INS.instFormal
			        {sign=s,entEnv=EE.empty,
				 rpath=InvPath.empty,region=region',
				 compInfo=compInfo};
			      ())
			else ()
                  in loop(rest, s::sigs, SE.bind(name, B.SIGbind s, env))
                 end

        in loop(sigbs, nil, SE.empty)
           handle EE.Unbound => (debugmsg("$elabDecl0: SigDec"); 
                                raise EE.Unbound)
       end (* SigDec sigbs *)                            

   | FsigDec fsigbs =>
       let val _ = debugmsg ">>elabFSigbs"

           fun loop([], fsigs, env) = 
                 let val _ = debugmsg "<<elabFSigbs"
                  in (A.FSIGdec(rev fsigs), M.EMPTYdec, env, EE.empty)
                 end

             | loop (fsigb::rest, fsigs, env) =
                 let val (name, def, region') =
                       case stripMarkFsigb(fsigb,region)
                         of (Fsigb{name=n,def=d},r) => (n, d, r)
                          | _ => bug "non fctSig bindings for FsigDec fsigbs"
                     val _ = debugmsg("--elabDecl0: fctsig "^S.name name)

                     val s = 
                       ES.elabFctSig {fsigexp=def, nameOp=SOME name, env=env0,
                                      entEnv=entEnv0, epContext=epContext, 
                                      region=region', compInfo=compInfo}
                  in loop(rest, s::fsigs, SE.bind(name, B.FSGbind s, env))
                 end

        in loop(fsigbs, nil, SE.empty)
           handle EE.Unbound => (debugmsg("$elabDecl0: FsigDec"); 
                                 raise EE.Unbound)
       end (* FsigDec fsigbs *)

   | LocalDec(decl_in,decl_out) =>
       let val top_in = EU.hasModules decl_in orelse EU.hasModules decl_out
	       (* if decl_in contains a functor declaration (at
		* any nesting depth, have to suppress ungeneralized
		* type variables to avoid bug 905/952.  Using 
		* EU.hasModules is a cheap conservative approximation to
		* checking for the presence of functor declarations, though
		* it excludes some legal SML 96 programs where structures
		* but not functors are present. *)
	   val (absyn_in, entDecl_in, env_in, entEnv_in) =
               elabDecl0(decl_in, env0, entEnv0, context, top_in,
			 epContext, rpath, region, compInfo)

           (*** DAVE? what is the right epContext to pass here? ***)
           val env = SE.atop(env_in,env0)
           val entEnv = EE.mark(mkStamp,EE.atop(entEnv_in,entEnv0))
           val (absyn_out, entDecl_out, env_out, entEnv_out) = 
               elabDecl0(decl_out, env, entEnv, context, top,
			 epContext, rpath, region, compInfo)

           val resAbsyn = A.LOCALdec(absyn_in,absyn_out)


           val (entDec, resEE) = 
             case context 
              of EU.INFCT _ => 
                   (localEntDec(entDecl_in,entDecl_out),
                    EE.mark(mkStamp,EE.atop(entEnv_out,entEnv_in)))
               | _ => (M.EMPTYdec, EE.empty)

        in (resAbsyn, entDec, env_out, resEE)
       end (* LocalDec(decl_in,decl_out) *)

   | SeqDec decls => 
       let val _ = debugmsg ">>elabSeqDec"

           fun loop([], asdecls, entdecls, env, entEnv) = 
                 let val resAbsyn = A.SEQdec(rev asdecls)
                     val (entDec', entEnv') = 
                         case context
			   of EU.INFCT _ => 
                               (seqEntDec(rev entdecls), entEnv)
			    | _ => (M.EMPTYdec, EE.empty)

                     val _ = debugPrint("elabseq - symbols: ", ED.ppSymList,
                                        ED.envSymbols env)
                     val _ = debugmsg "<<elabSeqDec"

                  in (resAbsyn, entDec', env, entEnv')
                 end

             | loop(decl::rest, asdecls, entDecls, env, entEnv) = 
                 let val env1 = SE.atop(env,env0)
                     val entEnv1 = EE.mark(mkStamp, EE.atop(entEnv,entEnv0))
                     val (absyn, entDecl, env', entEnv') = 
			 elabDecl0(decl, env1, entEnv1, context, top,
				   epContext, rpath, region, compInfo)
		     (* Consolidate env in case of large number of bindings
			in a single module *)
		     val env2 = SE.consolidateLazy (SE.atop(env',env))
                  in loop(rest, absyn::asdecls, entDecl::entDecls, env2,
                          EE.mark(mkStamp,EE.atop(entEnv',entEnv)))
                 end

        in loop(decls, nil, nil, SE.empty, EE.empty)
           handle EE.Unbound =>
	     (debugmsg("$elabDecl0: SeqDec"); 
	      raise EE.Unbound)
       end (* SeqDec decls *)

   | TypeDec tbs =>
       (*** ASSERT: the tycons declared are all DEFtycs ***)
       (let val (dec, env) =
                ET.elabTYPEdec(tbs,env0,rpath,region,compInfo)
            val tycs = case dec
			 of A.TYPEdec z => z
			  | _ => bug "elabDecl0 for TypeDec"
            val (entEnv, entDec) = 
              bindNewTycs(context, epContext, mkStamp, [], tycs, rpath,
			  error region)
         in (dec, entDec, env, entEnv)
        end
        handle EE.Unbound =>
	  (debugmsg("$elabDecl0: TypeDec");
	   raise EE.Unbound))

   | DatatypeDec (x as {datatycs,withtycs}) =>
      (case datatycs
	 of (Db{rhs=(Constrs _), ...}) :: _ =>
	      let val isFree = (* true if volatile, i.e. in domain of epcontext *)
                    (case context 
                      of EU.INFCT _ =>
                          (fn tyc => 
                            (case EPC.lookTycEntPath(epContext, MU.tycId tyc)
                              of SOME _ => true 
                               | _ => false))
                       | _ => (fn _ => false))

                  val (datatycs,withtycs,_,env) =
		    ET.elabDATATYPEdec(x, env0, [], EE.empty, isFree, rpath,
                                       region, compInfo)
		  val (entEnv, entDec) = 
		    bindNewTycs(context, epContext, mkStamp, 
                                datatycs, withtycs, rpath, error region)
		  val resDec = 
                    A.DATATYPEdec{datatycs=datatycs,withtycs=withtycs}
	       in (resDec, entDec, env, entEnv)
	      end

	  | (Db{tyc=name,rhs=Repl syms,tyvars=nil,lazyp=false}::nil) =>
	    let fun no_datatype () =
		    (error region EM.COMPLAIN
			   "rhs of datatype replication not a datatype"
			   EM.nullErrorBody;
		     (A.SEQdec[],M.ERRORdec,SE.empty,EE.empty))
	    in
 		case withtycs
		 of _::_ => 
                    (error region EM.COMPLAIN
		        "withtype not allowed in datatype replication"
			 EM.nullErrorBody;
                     (A.SEQdec[],M.ERRORdec,SE.empty,EE.empty))
		 | [] =>
		     let val tyc = L.lookTyc(env0, SP.SPATH syms, error region)
		      in case tyc
			  of T.GENtyc {stamp, arity, eq, path, stub,
			               kind = dt as (T.DATATYPE _)
				       } =>
			     let val dcons = TU.extractDcons tyc
				 val envDcons =
				     foldl (fn (d as T.DATACON{name,...},
						e)=>
					       SE.bind(name,
						       B.CONbind d, e))
 					   SE.empty dcons
				 val env = SE.bind(name, B.TYCbind tyc, 
						   envDcons)
				 val ev = mkStamp()
				 val tyc_id = MU.tycId tyc
				 val (ee_dec,ee_env) =
				     case context
				      of EU.INFCT _ => let
					     val texp =
						 case EPC.lookTycEntPath(epContext,tyc_id)
						  of NONE => M.CONSTtyc tyc
						   | SOME entPath =>
						     M.VARtyc entPath
			                 in (M.TYCdec(ev,texp),
					     EE.bind(ev,M.TYCent tyc,
						     EE.empty))
					 end
				       | _ => (M.EMPTYdec,EE.empty)
				 val tyc' = T.GENtyc{stamp=stamp, arity=arity, eq=eq,
						     path=InvPath.extend(InvPath.empty,name),
						     stub=stub, kind=dt}
				 val resDec =
				     A.DATATYPEdec{datatycs=[tyc' (* tyc *)],
						   withtycs=[]}
			     in
				 EPC.bindTycEntVar(epContext, tyc_id, ev);
				 (resDec, ee_dec, env, ee_env)
			     end
			   | _ => no_datatype ()
		     end
	    end

	  | _ => (error region EM.COMPLAIN
		   "argument type variables in datatype replication"
		   EM.nullErrorBody;
		  (A.SEQdec[],M.ERRORdec,SE.empty,EE.empty)))

   | AbstypeDec x =>
       (let val isFree = (* tycon in domain of epContext pathmap *)
              (case context 
                of EU.INFCT _ =>
                     (fn tyc => 
                       (case EPC.lookTycEntPath(epContext, MU.tycId tyc)
                         of SOME _ => true 
                          | _ => false))
                 | _ => (fn _ => false))

            val (decl, env', abstycs, withtycs) =
		case  EC.elabABSTYPEdec(x, env0, context, isFree, 
					rpath, region, compInfo) of
		    (d as A.ABSTYPEdec x, e) =>
		    (d, e, #abstycs x, #withtycs x)
		  | _ => bug "elabDecl0:AbstypeDec"
            (*
             * Potential bug: what about other datatype declarations within
             * the body of ABSTYPEdec ? they are local declarations; but
             * they may not be properly dealt with now ! (ZHONG)
             *)

            fun chkError () = !anyErrors
            (* note that transform is applied to decl before type checking *)
            val decl' = Typecheck.decType(SE.atop(env',env0), transform decl,
                                          top, error, chkError, region)
            val (entEnv, entDec) = 
              bindNewTycs(context, epContext, mkStamp, abstycs, withtycs,
			  rpath, error region)
         in (decl', entDec, env', entEnv)
        end
        handle EE.Unbound =>
	  (debugmsg("$elabDecl0: AbstypeDec");
	   raise EE.Unbound))

   | MarkDec(decl',region') =>
       elabDecl0(decl', env0, entEnv0, context, top,
                 epContext, rpath, region', compInfo)

   | dec =>  (* other core decls *)
       (let val isFree = 
             (case context 
               of EU.INFCT _ =>
                    (fn tyc => 
                       (case EPC.lookTycEntPath(epContext, MU.tycId tyc)
                         of SOME _ => true 
                          | _ => false))
                | _ => (fn _ => false))

            val (decl,env') = EC.elabDec(dec, env0, isFree, 
                                         rpath, region, compInfo)
              handle EE.Unbound => (debugmsg("$EC.elabDec"); raise EE.Unbound)
            val _ = debugmsg (">>elabDecl0.dec[after EC.elabDec: top=" 
                              ^ (Bool.toString top))
            val decl' = transform decl
            val _ = debugmsg ">>elabDecl0.dec[after transform]"
            fun chkError () = !anyErrors
            val decl'' = Typecheck.decType(SE.atop(env',env0), decl',
                                           top, error, chkError, region)
                         handle EE.Unbound => (debugmsg("$decType");
                                               raise EE.Unbound)
            val _ = debugmsg ">>elabDecl0.dec[after decType]"
         in (decl'', M.EMPTYdec, env', EE.empty)
        end handle EE.Unbound =>
	      (debugmsg("$elabDecl0: CoreDec"); raise EE.Unbound)))


(*** the top-level wrapper of the elabDecl0 function ***)
fun elabDecl {ast, statenv, entEnv, context, level,
              epContext, path, region, compInfo} =
    let val (resDec, _, senv, _) = 
	    elabDecl0(ast, statenv, entEnv, context, level,
                      epContext, path, region, compInfo)
     in {absyn=resDec, statenv=senv}
    end

end (* local *)
end (* functor ElabModFn *)
