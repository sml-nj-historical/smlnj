(* COPYRIGHT (c) 1996 Bell Laboratories. *)
(* moduleutil.sml *)

structure ModuleUtil : MODULEUTIL = 
struct

local structure S   = Symbol
      structure SP  = SymPath
      structure IP  = InvPath
      structure CVP = ConvertPaths
      structure EP  = EntPath
      structure EPC = EntPathContext
      structure A   = Access
      structure II  = InlInfo
      structure T   = Types
      structure TU  = TypesUtil
      structure V   = VarCon
      structure B   = Bindings
      structure EE  = EntityEnv
      structure ST  = Stamps
      structure M   = Modules
      structure MI  = ModuleId
      structure SE  = StaticEnv
      open Modules
in

(* debugging hooks *)
val say = Control.Print.say
val debugging = Control.CG.mudebugging (* ref false *)
fun debugmsg (msg: string) =
      if !debugging then (say msg; say "\n") else ()

fun bug s = ErrorMsg.impossible ("ModuleUtil: " ^ s)

(* 
 * Look up the entity corresponding to a given symbol in the `elements'
 * of a signature and the corresponding `entities' from a structure
 * realization.  The (dynamic) access fields of structures and
 * functors are adjusted before they are returned.  The static accesses
 * of types, structures, and functors are just returned.
 *
 * Used by the (structure and functor) matching functions.
 *)

exception Unbound of S.symbol

fun getSpec (elements, sym) = 
  let fun h [] = (debugmsg("$getSpec "^S.name sym); raise (Unbound sym))
        | h ((s, sp) :: elemr) = if S.eq(s, sym) then sp else h elemr
   in h elements
  end

(* 
 * the following might be used to speedup the signature lookup process 
 *
 * fun getSpec (elements, sym) = 
 *   Env.look(elements,sym)
 *   handle Env.Unbound => raise (Unbound sym)  
 *
 * we'll use more efficient represntations for elements in the future.
 *)  

(*** return the entity variable of a particular spec ***)
fun getSpecVar (STRspec{entVar,...}) = SOME entVar
  | getSpecVar (TYCspec{entVar,...}) = SOME entVar
  | getSpecVar (FCTspec{entVar,...}) = SOME entVar
  | getSpecVar _ = NONE

(*** The function getTyc is used in modules/sigmatch.sml only ***)
fun getTyc (elements, entEnv, sym) =
   case getSpec (elements, sym)
    of TYCspec{entVar,...} => (EE.lookTycEnt(entEnv,entVar), entVar)
     | _ => bug "getTyc: wrong spec"

(*** The function getStr is used in modules/sigmatch.sml only ***)
fun getStr (elements, entEnv, sym, dacc, dinfo) =
   case getSpec(elements, sym)
    of STRspec{sign, slot, def, entVar} => 
        (case EE.look(entEnv,entVar)
 	  of STRent entity => 
               (STR{sign = sign, rlzn = entity, access = A.selAcc(dacc,slot),
                    info = II.selInfo(dinfo, slot)}, entVar)
	   | _ => bug "getStr: bad entity")
     | _ => bug "getStr: wrong spec"

(*** The function getFct is used in modules/sigmatch.sml only ***)
fun getFct (elements, entEnv, sym, dacc, dinfo) =
   case getSpec(elements, sym)
    of FCTspec{sign, slot, entVar} => 
        (case EE.look(entEnv,entVar)
          of FCTent entity => 
               (FCT{sign = sign, rlzn = entity, access = A.selAcc(dacc,slot),
                    info = II.selInfo(dinfo, slot)}, entVar)
           | _ => bug "getFct: bad entity")
     | _ => bug "getFct: wrong spec"

val errorStrStamp = ST.special "ERRORstr"
val errorStrName = InvPath.IPATH[S.strSymbol "ERRORstr"]

fun getStrStamp(STR{rlzn={stamp,...},...}) = stamp
  | getStrStamp ERRORstr = errorStrStamp
  | getStrStamp _ = bug "getStrStamp"

fun getStrName(STR{rlzn={rpath,...},...}) = rpath
  | getStrName ERRORstr = errorStrName
  | getStrName _ = bug "getStrName"

fun getStrs(STR{sign=SIG{elements,...},rlzn={entities,...}, access, info}) =
      List.mapPartial
        (fn (sym,STRspec{sign,slot,def,entVar}) =>
             SOME(STR{sign = sign, 
                      rlzn = EE.lookStrEnt(entities,entVar),
                      access = A.selAcc(access, slot), 
                      info = II.selInfo(info, slot)})
          | _ => NONE) elements
  | getStrs ERRORstr = nil
  | getStrs _ = bug "getStrs"

fun getTycs(STR{sign=SIG{elements,...},rlzn={entities,...},...}) =
      let val tycvars = List.mapPartial
                          (fn (sym,TYCspec{entVar,...}) => SOME entVar
		            | _ => NONE) elements
       in List.map (fn tycVar => EE.lookTycEnt(entities,tycVar)) tycvars
      end
  | getTycs ERRORstr = nil
  | getTycs _ = bug "getTycs"

fun getSigSymbols(SIG{symbols,...}) = symbols
  | getSigSymbols _ = nil

fun getStrSymbols(STR{sign,...}) = getSigSymbols sign
  | getStrSymbols _ = nil

(*** Translate a tycon in a given entityEnv ***)
fun transTycon entEnv (T.PATHtyc{entPath,path,...}) =
      (EE.lookTycEP(entEnv,entPath)
	handle EE.Unbound =>
	  (debugmsg (String.concat["$transTycon ",
				   IP.toString path," ",
				   EP.entPathToString entPath]);
	   raise EE.Unbound))
  | transTycon _ tycon = tycon      


(*
 * Translate a type in a given entityEnv 
 *
 * We should never need to recurse inside each DEFtyc's body because
 * a DEFtycs is either rigid or has been relativized as a whole into 
 * a PATHtyc with an entPath somewhere before.
 *)
fun transType entEnv ty =
      TU.mapTypeFull (transTycon entEnv) ty
      handle EE.Unbound => (debugmsg "$transType"; raise EE.Unbound)

(*
val transTyconPhase = (Stats.makePhase "Compiler 033 4-transTycon") 
val transTycon = 
  fn x => fn y => (Stats.doPhase transTyconPhase (transTycon x) y)

val transTypePhase = (Stats.makePhase "Compiler 033 5-transType") 
val transType = 
  fn x => fn y => (Stats.doPhase transTypePhase (transType x) y)
*)

fun strDefToStr(CONSTstrDef str, _) = str
  | strDefToStr(VARstrDef(sign,entPath), entEnv) =
      STR{sign=sign,rlzn=EE.lookStrEP(entEnv,entPath),
          access=A.nullAcc, info=II.nullInfo}

(* 
 * two pieces of essential structure information gathered during
 * the environment lookup. SIGINFO is returned if the structure
 * being searched is a STRSIG; otherwise it return STRINFO.
 *)
datatype strInfo = SIGINFO of EP.entPath  (* reverse order! *)
                 | STRINFO of strEntity * A.access * II.inl_info

val bogusInfo = STRINFO (bogusStrEntity, A.nullAcc, II.nullInfo)

fun getStrElem (sym, sign as SIG{elements,...}, sInfo) = 
      (case getSpec(elements,sym)
        of STRspec{sign=subsig, slot, def, entVar} =>
            (let val newInfo = 
                  case sInfo
                   of SIGINFO ep => SIGINFO (entVar::ep)
                    | STRINFO (rlzn as {entities,...}, dacc, dinfo) => 
                          STRINFO(EE.lookStrEnt(entities,entVar), 
                                  A.selAcc(dacc,slot), II.selInfo(dinfo,slot))
              in (subsig, newInfo)
             end)
         | _ => bug "getStrElem: wrong spec case")

  | getStrElem (sym, sign, _) = (sign, bogusInfo)

fun getFctElem(sym, sign as SIG{elements,...}, 
               sinfo as STRINFO(rlzn as {entities,...}, dacc, dinfo)) = 
      (case getSpec(elements, sym)
        of FCTspec{sign=subfsig, entVar, slot} =>
             FCT{sign=subfsig, rlzn=EE.lookFctEnt(entities,entVar),
                 access=A.selAcc(dacc, slot), info=II.selInfo(dinfo, slot)}
         | _ => bug "mkFctVar - bad spec")

  | getFctElem _ = ERRORfct

fun mkTyc(sym, sp, SIG{elements,...}, sInfo) =
      (case getSpec (elements, sym)
        of TYCspec{spec,entVar=ev,repl,scope} => 
             (case sInfo
               of SIGINFO ep => 
                    T.PATHtyc{arity=TU.tyconArity spec, entPath=rev(ev::ep),
			      path=CVP.invertSPath sp}
                | STRINFO (rlzn as {entities,...}, _, _) =>
                    EE.lookTycEnt(entities, ev))
 
         | _ => bug "mkTyc: wrong spec case")

  | mkTyc _ = T.ERRORtyc

fun mkVal(sym, sp, sign as SIG{elements,...}, 
                   sInfo as STRINFO({entities,...}, dacc, dinfo)) : V.value = 
      (case getSpec(elements, sym)
        of VALspec{spec,slot} =>
             V.VAL(V.VALvar{access = A.selAcc(dacc,slot), 
                            info = II.selInfo(dinfo,slot), path = sp,
                            typ = ref(transType entities spec)})

         | CONspec{spec=T.DATACON{name, const, typ, rep, sign, lazyp}, slot} =>
             let val newrep =
                   case (rep, slot)
                    of (A.EXN _, SOME i) => A.EXN (A.selAcc(dacc,i))
                     | _ => rep

              in V.CON(T.DATACON{rep=newrep, name=name,
                                 typ=transType entities typ, 
                                 const=const, sign=sign, lazyp=lazyp})
             end

         | _ => bug "mkVal: wrong spec")

  | mkVal _ = V.VAL(V.ERRORvar)


fun mkStrBase(sym, sign, sInfo) = 
  let val (newsig, newInfo) = getStrElem(sym, sign, sInfo)
   in case newsig
       of ERRORsig => ERRORstr
	| _ =>
	  (case newInfo
	     of STRINFO(newrlzn, newacc, newinfo) => 
		 STR{sign=newsig, rlzn=newrlzn, access=newacc, info=newinfo}
	      | SIGINFO ep => STRSIG{sign=newsig, entPath=rev ep})
  end

fun mkStr(sym, _, sign, sInfo) = mkStrBase(sym, sign, sInfo)

fun mkStrDef(sym, _, sign, sInfo) = 
  let val (newsig, newInfo) = getStrElem(sym, sign, sInfo)
   in case newsig
        of ERRORsig => CONSTstrDef ERRORstr
	 | _ =>
	   (case newInfo
	      of STRINFO (newrlzn, newacc, newinfo) => 
		  CONSTstrDef(STR{sign=newsig, rlzn=newrlzn,
				  access=newacc, info=newinfo})
	       | SIGINFO ep => VARstrDef(newsig, rev ep))
  end

fun mkFct(sym, sp, sign, sInfo) = getFctElem(sym, sign, sInfo)

fun getPath makeIt (str, SP.SPATH spath, fullsp) =
  let fun loop([sym], sign, sInfo) = makeIt(sym, fullsp, sign, sInfo)
        | loop(sym::rest, sign, sInfo) = 
            let val (newsig, newsInfo) = getStrElem(sym, sign, sInfo)
             in loop(rest, newsig, newsInfo)
            end
        | loop _ = bug "getPath.loop"

   in case str 
       of STR{sign, rlzn, access, info} =>
            loop(spath, sign, STRINFO(rlzn, access, info))
        | STRSIG{sign, entPath} => 
            loop(spath, sign, SIGINFO (rev entPath))
        | _ => loop(spath, ERRORsig, bogusInfo)
  end

val getTycPath : M.Structure * SP.path * SP.path -> T.tycon =
      getPath mkTyc
val getValPath : M.Structure * SP.path * SP.path -> V.value =
      getPath mkVal
val getStrPath : M.Structure * SP.path * SP.path -> M.Structure =
      getPath mkStr
val getFctPath : M.Structure * SP.path * SP.path -> M.Functor =
      getPath mkFct
val getStrDef : M.Structure * SP.path * SP.path -> M.strDef =
      getPath mkStrDef

fun checkPathSig(sign: M.Signature, spath: SP.path) : S.symbol option =
    let val str = STRSIG{sign=sign,entPath=[]:EP.entPath}
        fun checkLast(sym,_,SIG{elements,...},_) = (getSpec(elements,sym);())
          | checkLast(sym,_,ERRORsig,_) = ()
     in getPath checkLast (str,spath,SP.empty);
	NONE
    end
    handle Unbound sym => SOME sym

fun errBinding sym =
  case S.nameSpace sym
   of S.VALspace => B.VALbind V.ERRORvar
    | S.TYCspace => B.TYCbind T.ERRORtyc
    | S.STRspace => B.STRbind M.ERRORstr
    | S.FCTspace => B.FCTbind M.ERRORfct
    | _ => raise (Unbound sym)

fun eqSign(SIG{stamp=s1,closed=true, ...},
           SIG{stamp=s2,closed=true, ...}) = ST.eq(s1,s2)
  | eqSign _ = false

fun eqOrigin(STR{rlzn={stamp=s1,...},...},
             STR{rlzn={stamp=s2,...},...}) = ST.eq(s1,s2)
  | eqOrigin _ = false


(* 
 * The following functions are used in CMStaticEnv and module elaboration
 * for building EntPathContexts.  They extract module ids from modules. 
 *)
fun tycId(T.GENtyc{stamp,...}) = ModuleId.TYCid stamp
  | tycId(T.DEFtyc{stamp,...}) = ModuleId.TYCid stamp
  | tycId _ = bug "tycId"

fun strId(STR{rlzn={stamp=rlznst,...},sign=SIG{stamp=sigst,...},...}) =
      MI.STRid{rlzn=rlznst,sign=sigst}
  | strId _ = bug "strId"

fun strId2(SIG{stamp=sigst,...}, {stamp=rlznst,...} : strEntity) =  
      MI.STRid{rlzn=rlznst,sign=sigst}
  | strId2 _ = bug "strId2"

fun fsigId(FSIG{paramsig=SIG{stamp=sp,...},bodysig=SIG{stamp=sb,...},...}) =
      MI.FSIGid{paramsig=sp,bodysig=sb}
  | fsigId _ = bug "fsigId"

fun fctId(FCT{rlzn={stamp,...},sign, ...}) =
      MI.FCTid{rlzn=stamp,sign=fsigId sign}
  | fctId _ = bug "fctId"

fun fctId2(sign, {stamp,...} : fctEntity) = 
      MI.FCTid{rlzn=stamp,sign=fsigId sign}

(*
 * The reason that relativizeType does not need to get inside 
 * DEFtyc is because of our assumptions that the body in DEFtyc
 * has already been relativized, when DEFtyc is elaborated; 
 * otherwise, this DEFtyc must be a rigid tycon.
 *)
fun relativizeTyc epContext : T.tycon -> T.tycon * bool = 
  let fun mapTyc(tyc as (T.GENtyc{stamp,...} | T.DEFtyc{stamp,...})) = 
	    let val tyc_id = ModuleId.TYCid stamp
	     in debugmsg ("mapTyc: "^ModuleId.idToString tyc_id);
	        case EPC.lookPath(epContext,tyc_id)
		  of NONE => (debugmsg "tyc not mapped 1"; (tyc,false))
		   | SOME entPath =>
		     let val tyc' = T.PATHtyc{arity=TU.tyconArity tyc,
					      entPath=entPath,
					      path=TU.tycPath tyc}
		      in debugmsg("tyc mapped: "^
				 Symbol.name(TypesUtil.tycName tyc'));
			 (tyc',true)
		     end
	    end
	  | mapTyc(tyc as T.PATHtyc _) =
             (* assume this is a local tycon within the current signature *)
	     (debugmsg "tyc not mapped 2";
	      (tyc,true))
	  | mapTyc tyc = (debugmsg "tyc not mapped 3"; (tyc,false))

       fun mapTyc' tyc = 
	    (debugmsg("mapTyc': "^(Symbol.name(TypesUtil.tycName tyc)));
	     mapTyc tyc)
   in mapTyc'
  end
        
fun relativizeType epContext ty : T.ty * bool =
    let val relative = ref false
	fun vizTyc tyc = 
	    let val (tyc',rel) = relativizeTyc epContext tyc
	     in relative := (!relative orelse rel);
		tyc'
	    end
     in (TU.mapTypeFull vizTyc ty, !relative)
    end


(*
val relativizeTypePhase = (Stats.makePhase "Compiler 033 2-vizType") 
val relativizeType = 
  fn x => fn y =>
   (Stats.doPhase relativizeTypePhase (relativizeType x) y)

*)

(* 
 * getBinding(sym,str): return binding for element sym of structure str
 *  - used only inside the function openStructure
 *  - raises ModuleUtil.Unbound if sym not found in sig 
 *)
fun getBinding (sym, str as STR{sign as SIG{elements,...},
                                rlzn as {entities,...}, 
                                access=dacc, info=dinfo}) = 
     let val sinfo = STRINFO(rlzn, dacc, dinfo)
      in case S.nameSpace sym
          of S.VALspace => 
               (case mkVal(sym, SP.SPATH[sym], sign, sinfo)
                 of V.VAL v => B.VALbind v
                  | V.CON d => B.CONbind d)

           | S.TYCspace => B.TYCbind(mkTyc(sym, SP.SPATH[sym], sign, sinfo))
           | S.STRspace => B.STRbind(mkStrBase(sym, sign, sinfo))
           | S.FCTspace => B.FCTbind(getFctElem(sym, sign, sinfo))
           | sp => (debugmsg ("getBinding: "^S.symbolToString sym);
  		    raise (Unbound sym))
     end

  | getBinding (sym, STRSIG{sign as SIG{elements, ...},entPath=ep}) = 
     let val sinfo = SIGINFO(rev ep)
      in case S.nameSpace sym
          of S.TYCspace => B.TYCbind(mkTyc(sym, SP.SPATH[sym], sign, sinfo))
           | S.STRspace => B.STRbind(mkStrBase(sym, sign, sinfo))
           | _ => (debugmsg ("getBinding: "^S.symbolToString sym);
                   raise (Unbound sym))
     end 
         
  | getBinding (sym, STR{sign=ERRORsig,...}) = errBinding sym
  | getBinding (sym, ERRORstr) = errBinding sym
  | getBinding _ = bug "getBinding - bad arg"

fun openStructure(env: SE.staticEnv, str) =
  let fun look sym = getBinding(sym,str) handle Unbound _ => raise SE.Unbound
      val symbols = getStrSymbols str
      val genSyms = (fn () => symbols)
      val nenv = SE.special(look, genSyms)
   in SE.atop(nenv,env)
  end

(** extract inl_info from a list of bindings *)
fun extractInfo(B.STRbind(M.STR{info, ...})) = info
  | extractInfo(B.FCTbind(M.FCT{info, ...})) = info
  | extractInfo(B.VALbind(V.VALvar{info, ...})) = info
  | extractInfo(B.CONbind _) = II.nullInfo
  | extractInfo(B.STRbind _) = II.nullInfo
  | extractInfo(B.FCTbind _) = II.nullInfo
  | extractInfo _ = bug "unexpected binding in extractInfo"

(* extract all signature names from a structure --
 *  doesn't look into functor components *)
fun getSignatureNames(STR{sign,...} | STRSIG{sign,...}) =
    let fun sigNames(SIG{name,elements,...},names) =
	    foldl (fn ((_,STRspec{sign,...}),ns) =>
		       sigNames(sign, ns)
		    | (_,ns) => ns)
		  (case name of SOME n => n::names | NONE => names)
		  elements
	  | sigNames(ERRORsig,names) = names
	fun removeDups (x::(rest as y::_),z) = 
	    if S.eq(x,y) then removeDups(rest,z) else removeDups(rest,x::z)
	  | removeDups (x::nil,z) = x::z
	  | removeDups (nil,z) = z
     in removeDups(ListMergeSort.sort S.symbolGt(sigNames(sign,nil)), nil)
    end
  | getSignatureNames(ERRORstr) = nil

end (* local *)
end (* structure ModuleUtil *)

(*
 * $Log: moduleutil.sml,v $
 * Revision 1.4  1998/09/30 19:17:48  dbm
 * change in types of TYCspec (bug 1432) and TYCdef
 *
 * Revision 1.3  1998/05/23 14:10:10  george
 *   Fixed RCS keyword syntax
 *
 *)
