(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* translate.sml *)

signature TRANSLATE = 
sig

  (* Invariant: transDec always applies to a top-level absyn declaration *) 
  val transDec : { rootdec: Absyn.dec,
		   exportLvars: Access.lvar list,
                   oldenv: StaticEnv.staticEnv,
                   env: StaticEnv.staticEnv,
		   cproto_conv: string,
		   compInfo: Absyn.dec CompInfo.compInfo }
                 -> {flint: FLINT.prog,
                     imports: (PersStamps.persstamp 
                               * ImportTree.importTree) list}

end (* signature TRANSLATE *)

structure Translate : TRANSLATE =
struct

local structure B  = Bindings
      structure BT = BasicTypes
      structure DA = Access
      structure DI = DebIndex
      structure EM = ErrorMsg
      structure LT = PLambdaType   (* = LtyExtern *)
      structure M  = Modules
      structure MC = MatchComp
      structure PO = PrimOp
      structure PP = PrettyPrintNew
      structure PU = PPUtilNew
      structure S  = Symbol
      structure SP = SymPath
      structure LN = LiteralToNum
      structure TT = TransTypes
      structure TP = Types
      structure TU = TypesUtil
      structure V  = VarCon
      structure EU = ElabUtil

      structure IIMap = RedBlackMapFn (type ord_key = IntInf.int
					val compare = IntInf.compare)

      open Absyn PLambda 
in 

(****************************************************************************
 *                   CONSTANTS AND UTILITY FUNCTIONS                        *
 ****************************************************************************)

val debugging = ref false
fun bug msg = EM.impossible("Translate: " ^ msg)
val say = Control.Print.say

fun debugmsg (msg : string) =
    if !debugging then (say msg; say "\n") else ()

val ppDepth = Control.Print.printDepth

val with_pp = PP.with_default_pp

fun ppType ty =
    ElabDebug.withInternals
     (fn () => ElabDebug.debugPrint debugging
		("type: ",PPType.ppType StaticEnv.empty, ty))

fun ppLexp lexp = 
    PP.with_default_pp(fn s => PPLexp.ppLexp 20 s lexp)

fun ident x = x
val unitLexp = RECORD []

fun getNameOp p = if SP.null p then NONE else SOME(SP.last p)

type pid = PersStamps.persstamp

(** old-style fold for cases where it is partially applied *)
fun fold f l init = foldr f init l

(** sorting the record fields for record types and record expressions *)
fun elemgtr ((LABEL{number=x,...},_),(LABEL{number=y,...},_)) = (x>y)
fun sorted x = ListMergeSort.sorted elemgtr x 
fun sortrec x = ListMergeSort.sort elemgtr x

(** check if an access is external *)
fun extern (DA.EXTERN _) = true
  | extern (DA.PATH(a, _)) = extern a
  | extern _ = false

(** an exception raised if coreEnv is not available *)
exception NoCore

(****************************************************************************
 *                          MAIN FUNCTION                                   *
 *                                                                          *
 *  val transDec : Absyn.dec * Access.lvar list                             *
 *                 * StaticEnv.staticEnv * CompBasic.compInfo               *
 *                 -> {flint: FLINT.prog,                                   *
 *                     imports: (PersStamps.persstamp                       *
 *                               * ImportTree.importTree) list}             *
 ****************************************************************************)

fun transDec
	{ rootdec, exportLvars, oldenv, env, cproto_conv,
	 compInfo as {errorMatch,error,...}: Absyn.dec CompInfo.compInfo } =
let 

(* We take mkLvar from compInfo.  This should answer Zhong's question... *)
(*
(*
 * MAJOR CLEANUP REQUIRED ! The function mkv is currently directly taken 
 * from the LambdaVar module; I think it should be taken from the 
 * "compInfo". Similarly, should we replace all mkLvar in the backend
 * with the mkv in "compInfo" ? (ZHONG)
 *)
val mkv = LambdaVar.mkLvar 
fun mkvN NONE = mkv()
  | mkvN (SOME s) = LambdaVar.namedLvar s
*)

val mkvN = #mkLvar compInfo
fun mkv () = mkvN NONE


(** generate the set of ML-to-FLINT type translation functions *)
val {tpsKnd, tpsTyc, toTyc, toLty, strLty, fctLty, markLBOUND} =
    TT.genTT()
fun toTcLt d = (toTyc d, toLty d)

(** translating the typ field in DATACON into lty; constant datacons 
    will take ltc_unit as the argument *)
fun toDconLty d ty =
  (case ty 
    of TP.POLYty{sign, tyfun=TP.TYFUN{arity, body}} =>
         if BT.isArrowType body then toLty d ty
         else toLty d (TP.POLYty{sign=sign, 
                               tyfun=TP.TYFUN{arity=arity,
                                              body=BT.-->(BT.unitTy, body)}})
     | _ => if BT.isArrowType ty then toLty d ty
            else toLty d (BT.-->(BT.unitTy, ty)))

(** the special lookup functions for the Core environment *)
(* DBM: not used -- superceded by CoreAccess *)
fun coreLookup(id, env) = 
  let val sp = SymPath.SPATH [CoreSym.coreSym, S.varSymbol id]
      val err = fn _ => fn _ => fn _ => raise NoCore
   in Lookup.lookVal(env, sp, err)
  end

fun CON' ((_, DA.REF, lt), ts, e) = APP (PRIM (PO.MAKEREF, lt, ts), e)
  | CON' ((_, DA.SUSP (SOME(DA.LVAR d, _)), lt), ts, e) =
      let val v   = mkv ()
          val fe = FN (v, LT.ltc_tuple [], e)
       in APP(TAPP (VAR d, ts), fe)
      end
  | CON' x = CON x

(*
 * The following code implements the exception tracking and 
 * errormsg reporting. 
 *)

local val region = ref(0,0)
      val markexn = PRIM(PO.MARKEXN,
		      LT.ltc_parrow(LT.ltc_tuple [LT.ltc_exn, LT.ltc_string],
				    LT.ltc_exn), [])
in 

fun withRegion loc f x =
  let val r = !region
   in (region := loc; f x before region:=r) 
      handle e => (region := r; raise e)
  end

fun mkRaise(x, lt) = 
  let val e = if !Control.trackExn 
              then APP(markexn, RECORD[x, STRING(errorMatch(!region))])
              else x
   in RAISE(e, lt)
  end 

fun complain s = error (!region) s
fun repErr x = complain EM.COMPLAIN x EM.nullErrorBody
fun repPolyEq () = 
    if !Control.polyEqWarn then complain EM.WARN "calling polyEqual" EM.nullErrorBody
    else ()

fun repWarn msg = complain EM.WARN msg EM.nullErrorBody 

(** This may shadow previous definition of mkv ... this version reports the
    site of introduction of the lvar *)
fun mkv () = mkvN NONE 

end (* markexn-local *)

(***************************************************************************
 *          SHARING AND LIFTING OF STRUCTURE IMPORTS AND ACCESSES          *
 ***************************************************************************)

exception HASHTABLE
type key = int

(** hashkey of accesspath + accesspath + resvar *)
type info = (key * int list * lvar) 
val hashtable : info list IntHashTable.hash_table =
    IntHashTable.mkTable(32,HASHTABLE)    
fun hashkey l = foldr (fn (x,y) => ((x * 10 + y) mod 1019)) 0 l

fun buildHdr v = 
  let val info = IntHashTable.lookup hashtable v
      fun h((_, l, w), hdr) = 
             let val le = foldl (fn (k,e) => SELECT(k,e)) (VAR v) l
	      in fn e => hdr(LET(w, le, e))
	     end
   in foldr h ident info
  end handle _ => ident

fun bindvar (v, [], _) =  v
  | bindvar (v, l, nameOp) = 
      let val info = (IntHashTable.lookup hashtable v) handle _ => []
          val key = hashkey l
          fun h [] =  
                let val u = mkvN nameOp
                 in IntHashTable.insert hashtable (v,(key,l,u)::info); u
                end
            | h((k',l',w)::r) = 
                if (k' = key) then (if (l'=l) then w else h r) else h r    
       in h info 
      end

datatype pidInfo = ANON of (int * pidInfo) list
                 | NAMED of lvar * lty * (int * pidInfo) list

fun mkPidInfo (t, l, nameOp) = 
  let val v = mkvN nameOp
      fun h [] = NAMED(v, t, [])
        | h (a::r) = ANON [(a, h r)]
   in (h l, v)
  end

fun mergePidInfo (pi, t, l, nameOp) = 
  let fun h (z as NAMED(v,_,_), []) = (z, v)
        | h (ANON xl, [])  = 
              let val v = mkvN nameOp
               in (NAMED(v, t, xl), v)
              end
        | h (z, a::r) = 
              let val (xl, mknode) = 
                    case z of ANON c => (c, ANON)
                            | NAMED (v,tt,c) => (c, fn x => NAMED(v,tt,x))

                  fun dump ((np, v), z, y) = 
                        let val nz = (a, np)::z
                         in (mknode((rev y) @ nz), v)
                        end

                  fun look ([], y) = dump(mkPidInfo(t, r, nameOp), [], y)
                    | look (u as ((x as (i,pi))::z), y) = 
                        if i < a then look(z, x::y)
                        else if i = a then dump(h(pi, r), z, y)
                             else dump(mkPidInfo(t, r, nameOp), u, y)

               in look(xl, [])
              end
   in h(pi, l)
  end (* end of mergePidInfo *)

(** a map that stores information about external references *)
val persmap = ref (PersMap.empty : pidInfo PersMap.map)

fun mkPid (pid, t, l, nameOp) =
    case PersMap.find (!persmap, pid)
      of NONE => 
	  let val (pinfo, var) = mkPidInfo (t, l, nameOp)
	   in persmap := PersMap.insert(!persmap, pid, pinfo);
	      var
	  end
       | SOME pinfo =>
	  let val (npinfo, var) = mergePidInfo (pinfo, t, l, nameOp)
	      fun rmv (key, map) = 
		  let val (newMap, _) = PersMap.remove(map, key) 
		  in newMap
		  end handle e => map
	   in persmap := PersMap.insert(rmv(pid, !persmap), pid, npinfo);
	      var
	  end

val iimap = ref (IIMap.empty : lvar IIMap.map)

fun getII n =
    case IIMap.find (!iimap, n) of
	SOME v => v
      | NONE => let val v = mkv ()
		in
		    iimap := IIMap.insert (!iimap, n, v);
		    v
		end

(** converting an access w. type into a lambda expression *)
fun mkAccT (p, t, nameOp) = 
  let fun h(DA.LVAR v, l) = bindvar(v, l, nameOp)
        | h(DA.EXTERN pid, l) = mkPid(pid, t, l, nameOp)
        | h(DA.PATH(a,i), l) = h(a, i::l)
        | h _ = bug "unexpected access in mkAccT"
   in VAR (h(p, []))
  end (* new def for mkAccT *)

(** converting an access into a lambda expression *)
fun mkAcc (p, nameOp) = 
  let fun h(DA.LVAR v, l) = bindvar(v, l, nameOp)
        | h(DA.PATH(a,i), l) = h(a, i::l)
        | h _ = bug "unexpected access in mkAcc"
   in VAR (h(p, []))
  end (* new def for mkAcc *)

(* 
 * These two functions are major gross hacks. The NoCore exceptions would
 * be raised when compiling boot/dummy.sml, boot/assembly.sig, and 
 * boot/core.sml; the assumption is that the result of coreExn and coreAcc
 * would never be used when compiling these three files. A good way to
 * clean up this is to put all the core constructors and primitives into
 * the primitive environment. (ZHONG)
 *)
exception NoCore

fun coreExn id =
    (case CoreAccess.getCon' (fn () => raise NoCore) (oldenv, id) of
	 TP.DATACON { name, rep as DA.EXN _, typ, ... } =>
         let val nt = toDconLty DI.top typ
             val nrep = mkRep(rep, nt, name)
	     val _ = debugmsg ">>coreExn in translate.sml: "
	     (* val _ = PPLexp.printLexp (CON'((name, nrep, nt), [], unitLexp))
	     val _ = print "\n" *)
         in CON'((name, nrep, nt), [], unitLexp)
         end
       | _ => bug "coreExn in translate")
    handle NoCore => (say "WARNING: no Core access\n"; INT 0)

and coreAcc id =
    (case CoreAccess.getVar' (fn () => raise NoCore) (oldenv, id) of
	 V.VALvar { access, typ, path, ... } =>
	 mkAccT(access, toLty DI.top (!typ), getNameOp path)
       | _ => bug "coreAcc in translate")
    handle NoCore => (say "WARNING: no Core access\n"; INT 0)

(** expands the flex record pattern and convert the EXN access pat *)
(** internalize the conrep's access, always exceptions *)
and mkRep (rep, lt, name) = 
  let fun g (DA.LVAR v, l, t)  = bindvar(v, l, SOME name)
        | g (DA.PATH(a, i), l, t) = g(a, i::l, t)
        | g (DA.EXTERN p, l, t) = mkPid(p, t, l, SOME name)
        | g _ = bug "unexpected access in mkRep"

   in case rep
       of (DA.EXN x) => 
             let val (argt, _) = LT.ltd_parrow lt
              in DA.EXN (DA.LVAR (g(x, [], LT.ltc_etag argt)))
             end
        | (DA.SUSP NONE) =>  (* a hack to support "delay-force" primitives *)
             (case (coreAcc "delay", coreAcc "force")
               of (VAR x, VAR y) => DA.SUSP(SOME (DA.LVAR x, DA.LVAR y))
                | _ => bug "unexpected case on conrep SUSP 1")
        | (DA.SUSP (SOME _)) => bug "unexpected case on conrep SUSP 2"
        | _ => rep 
  end

(** converting a value of access+prim into the lambda expression
 ** [KM???} But it is ignoring the prim argument!!! 
 **)
fun mkAccInfo (acc, prim, getLty, nameOp) = 
  if extern acc then mkAccT(acc, getLty(), nameOp) else mkAcc (acc, nameOp)

fun fillPat(pat, d) = 
  let fun fill (CONSTRAINTpat (p,t)) = fill p
        | fill (LAYEREDpat (p,q)) = LAYEREDpat(fill p, fill q)
        | fill (RECORDpat {fields, flex=false, typ}) =
            RECORDpat{fields = map (fn (lab, p) => (lab, fill p)) fields,
                      typ = typ, flex = false}
        | fill (pat as RECORDpat {fields, flex=true, typ}) =
            let exception DontBother
                val fields' = map (fn (l,p) => (l, fill p)) fields

                fun find (t as TP.CONty(TP.RECORDtyc labels, _)) = 
                             (typ := t; labels)
                  | find _ = (complain EM.COMPLAIN "unresolved flexible record"
                              (fn ppstrm => 
                                    (PP.newline ppstrm;
                                     PP.string ppstrm "pattern: ";
                                     PPAbsyn.ppPat env ppstrm
                                        (pat,!Control.Print.printDepth)));
                               raise DontBother)

                fun merge (a as ((id,p)::r), lab::s) =
                      if S.eq(id,lab) then (id,p) :: merge(r,s)
                                      else (lab,WILDpat) :: merge(a,s)
                  | merge ([], lab::s) = (lab,WILDpat) :: merge([], s)
                  | merge ([], []) = []
                  | merge _ = bug "merge in translate"

             in RECORDpat{fields = merge(fields', 
                                         find(TU.headReduceType (!typ))),
                          flex = false, typ = typ}
                handle DontBother => WILDpat
            end
        | fill (VECTORpat(pats,ty)) = VECTORpat(map fill pats, ty)
        | fill (ORpat(p1, p2)) = ORpat(fill p1, fill p2)
        | fill (CONpat(TP.DATACON{name,const,typ,rep,sign,lazyp}, ts)) = 
            CONpat(TP.DATACON{name=name, const=const, typ=typ, lazyp=lazyp,
                              sign=sign,rep=mkRep(rep,toDconLty d typ,name)},
                   ts)
        | fill (APPpat(TP.DATACON{name,const,typ,rep,sign,lazyp}, ts, pat)) = 
            APPpat(TP.DATACON{name=name, const=const, typ=typ,
                              sign=sign, lazyp=lazyp,
                              rep=mkRep(rep, toDconLty d typ, name)},
                   ts, fill pat)
        | fill xp = xp

   in fill pat
  end (* function fillPat *)

(** The runtime polymorphic equality and string equality dictionary. *)
val eqDict =
  let val strEqRef : lexp option ref = ref NONE
      val polyEqRef : lexp option ref = ref NONE
      val intInfEqRef : lexp option ref = ref NONE

      fun getStrEq () = 
        (case (!strEqRef) 
          of SOME e => e
           | NONE => (let val e = coreAcc "stringequal"
                       in strEqRef := (SOME e); e
                      end))

      fun getIntInfEq () =		(* same as polyeq, but silent *)
	  case !intInfEqRef of
	      SOME e => e
	    | NONE => let val e =
			      TAPP (coreAcc "polyequal",
				    [toTyc DI.top BT.intinfTy])
		      in
			  intInfEqRef := SOME e; e
		      end

      fun getPolyEq () = 
        (repPolyEq();
	 case (!polyEqRef) 
          of SOME e => e
           | NONE => (let val e = coreAcc "polyequal"
                       in polyEqRef := (SOME e); e
                      end))
   in {getStrEq=getStrEq, getIntInfEq=getIntInfEq, getPolyEq=getPolyEq}
  end

val eqGen = PEqual.equal (eqDict, env) 

(***************************************************************************
 *                                                                         *
 * Translating the primops; this should be moved into a separate file      *
 * in the future. (ZHONG)                                                  *
 *                                                                         *
 ***************************************************************************)

val lt_tyc = LT.ltc_tyc
val lt_arw = LT.ltc_parrow
val lt_tup = LT.ltc_tuple
val lt_int = LT.ltc_int
val lt_int32 = LT.ltc_int32
val lt_bool = LT.ltc_bool
val lt_unit = LT.ltc_unit

val lt_ipair = lt_tup [lt_int, lt_int]
val lt_i32pair = lt_tup [lt_int32, lt_int32]
val lt_icmp = lt_arw (lt_ipair, lt_bool)
val lt_ineg = lt_arw (lt_int, lt_int)
val lt_intop = lt_arw (lt_ipair, lt_int)
val lt_u_u = lt_arw (lt_unit, lt_unit)

val boolsign = BT.boolsign
val (trueDcon', falseDcon') = 
  let val lt = LT.ltc_parrow(LT.ltc_unit, LT.ltc_bool)
      fun h (TP.DATACON{name,rep,typ,...}) = (name, rep, lt)
   in (h BT.trueDcon, h BT.falseDcon)
  end

val trueLexp = CON(trueDcon', [], unitLexp) 
val falseLexp = CON(falseDcon', [], unitLexp)

fun COND(a,b,c) =
  SWITCH(a,boolsign, [(DATAcon(trueDcon', [], mkv()),b),
                      (DATAcon(falseDcon', [], mkv()),c)], NONE)

fun composeNOT (eq, t) =  
  let val v = mkv()
      val argt = lt_tup [t, t]
   in FN(v, argt, COND(APP(eq, VAR v), falseLexp, trueLexp))
  end

fun cmpOp p = PRIM(p, lt_icmp, [])
fun inegOp p = PRIM(p, lt_ineg, [])

val LESSU = PO.CMP{oper=PO.LTU, kind=PO.UINT 31}

val lt_len = LT.ltc_poly([LT.tkc_mono], [lt_arw(LT.ltc_tv 0, lt_int)])
val lt_upd = 
  let val x = LT.ltc_ref (LT.ltc_tv 0)
   in LT.ltc_poly([LT.tkc_mono], 
                  [lt_arw(lt_tup [x, lt_int, LT.ltc_tv 0], LT.ltc_unit)])
  end

fun lenOp(tc) = PRIM(PO.LENGTH, lt_len, [tc])

fun rshiftOp k  = PO.ARITH{oper=PO.RSHIFT, overflow=false,  kind=k}
fun rshiftlOp k = PO.ARITH{oper=PO.RSHIFTL, overflow=false, kind=k}
fun lshiftOp k = PO.ARITH{oper=PO.LSHIFT,  overflow=false, kind=k}

fun lword0 (PO.UINT 31) = WORD 0w0  
  | lword0 (PO.UINT 32) = WORD32 0w0
  | lword0 _  = bug "unexpected case in lword0"

fun baselt (PO.UINT 31) = lt_int
  | baselt (PO.UINT 32) = lt_int32
  | baselt _  = bug "unexpected case in baselt"

fun shiftTy k = 
  let val elem = baselt k
      val tupt = lt_tup [elem, lt_int] 
   in lt_arw(tupt, elem)
  end 

fun inlineShift(shiftOp, kind, clear) = 
  let fun shiftLimit (PO.UINT lim | PO.INT lim) = WORD(Word.fromInt lim)
        | shiftLimit _ = bug "unexpected case in shiftLimit"

      val p = mkv() val vp = VAR p
      val w = mkv() val vw = VAR w
      val cnt = mkv() val vcnt = VAR cnt

      val argt = lt_tup [baselt(kind), lt_int]
      val cmpShiftAmt = 
	PRIM(PO.CMP{oper=PO.LEU, kind=PO.UINT 31}, lt_icmp, [])
   in FN(p, argt,
         LET(w, SELECT(0, vp),
             LET(cnt, SELECT(1, vp),
                 COND(APP(cmpShiftAmt, RECORD [shiftLimit(kind), vcnt]),
                      clear vw, 
		      APP(PRIM(shiftOp(kind), shiftTy(kind), []),
			  RECORD [vw, vcnt])))))
  end

fun inlops nk = let
    val (lt_arg, zero, overflow) =
	case nk of
	    PO.INT 31 => (LT.ltc_int, INT 0, true)
	  | PO.UINT 31 => (LT.ltc_int, WORD 0w0, false)
	  | PO.INT 32 => (LT.ltc_int32, INT32 0, true)
	  | PO.UINT 32 => (LT.ltc_int32, WORD32 0w0, false)
	  | PO.FLOAT 64 => (LT.ltc_real, REAL "0.0", false)
	  | _ => bug "inlops: bad numkind"
    val lt_argpair = lt_tup [lt_arg, lt_arg]
    val lt_cmp = lt_arw (lt_argpair, lt_bool)
    val lt_neg = lt_arw (lt_arg, lt_arg)
    val less = PRIM (PO.CMP { oper = PO.<, kind = nk }, lt_cmp, [])
    val greater = PRIM (PO.CMP { oper = PO.>, kind = nk }, lt_cmp, [])
    val negate =
	PRIM (PO.ARITH { oper = PO.~, overflow = overflow, kind = nk },
	      lt_neg, [])
in
    { lt_arg = lt_arg, lt_argpair = lt_argpair, lt_cmp = lt_cmp,
      less = less, greater = greater,
      zero = zero, negate = negate }
end

fun inlminmax (nk, ismax) = let
    val { lt_argpair, less, greater, lt_cmp, ... } = inlops nk
    val x = mkv () and y = mkv () and z = mkv ()
    val cmpop = if ismax then greater else less
    val elsebranch =
	case nk of
	    PO.FLOAT _ => let
		(* testing for NaN *)
		val fequal =
		    PRIM (PO.CMP { oper = PO.EQL, kind = nk }, lt_cmp, [])
	    in
		COND (APP (fequal, RECORD [VAR y, VAR y]), VAR y, VAR x)
	    end
	  | _ => VAR y
in
    FN (z, lt_argpair,
	LET (x, SELECT (0, VAR z),
	     LET (y, SELECT (1, VAR z),
		  COND (APP (cmpop, RECORD [VAR x, VAR y]),
			VAR x, elsebranch))))
end

fun inlabs nk = let
    val { lt_arg, greater, zero, negate, ... } = inlops nk
    val x = mkv ()
in
    FN (x, lt_arg,
	COND (APP (greater, RECORD [VAR x, zero]),
	      VAR x, APP (negate, VAR x)))
end

(** inl_infPrec : string * string * PrimOp.primop * Lty.lty * bool -> PLambda.lexp

    Precision converting translation using a conversion
    primitive named in the second argument. 

    Examples: 
	     inl_infPrec ("EXTEND_INF", "finToInf", p, lt, false) 
             inl_infPrec ("COPY", "finToInf", p, lt, false) 

	system/smlnj/init/core-intinf.sml:51:    val finToInf  : int32 * bool -> intinf
 *)
fun inlToInfPrec (opname: string, coerceFnName: string, primop, primoplt) =
   let 
	val (orig_arg_lt, res_lt) =
		case LT.ltd_arrow primoplt handle LT.DeconExn => bug "inlToInfPrec" of
	    	(_, [a], [r]) => (a, r)
	  	| _ => bug ("unexpected type of " ^ opname)
    	val extra_arg_lt =
            if coerceFnName = "finToInf" then
              LT.ltc_arrow(LT.ffc_var(true,false),
                           [LT.ltc_int32 ,LT.ltc_bool], [res_lt])
            else LT.ltc_parrow(LT.ltc_int32, res_lt)
        val new_arg_lt = LT.ltc_tuple [orig_arg_lt, extra_arg_lt]
        val new_lt = LT.ltc_parrow (new_arg_lt, res_lt )
        val x = mkv ()
    in
       FN (x, orig_arg_lt,
	  APP (PRIM (primop, new_lt, []),
	       RECORD [VAR x, coreAcc coerceFnName]))
    end

fun inlFromInfPrec (opname, coerceFnName, primop, primoplt) =    
    let 
	val (orig_arg_lt, res_lt) =
		case LT.ltd_arrow primoplt handle LT.DeconExn => bug "inlFromInfPrec" of
	    	(_, [a], [r]) => (a, r)
	  	| _ => bug ("unexpected type of " ^ opname)
    	val extra_arg_lt =
		LT.ltc_parrow (orig_arg_lt, LT.ltc_int32)
        val new_arg_lt = LT.ltc_tuple [orig_arg_lt, extra_arg_lt]
        val new_lt = LT.ltc_parrow (new_arg_lt, res_lt )
        val x = mkv ()
    in
       FN (x, orig_arg_lt,
	  APP (PRIM (primop, new_lt, []),
	       RECORD [VAR x, coreAcc coerceFnName]))
    end
    
  
fun inl_infPrec (opname, coerceFnName, primop, primoplt, is_from_inf) = let
    val (orig_arg_lt, res_lt) =
	case LT.ltd_arrow primoplt handle LT.DeconExn => bug "inl_infPrec" of
	    (_, [a], [r]) => (a, r)
	  | _ => bug ("unexpected type of " ^ opname)
    val extra_arg_lt =
	LT.ltc_parrow (if is_from_inf then (orig_arg_lt, LT.ltc_int32)
		       else (LT.ltc_int32, res_lt (* orig_arg_lt *) ))
    val new_arg_lt = LT.ltc_tuple [orig_arg_lt, extra_arg_lt]
    val new_lt = LT.ltc_parrow (new_arg_lt, res_lt )
    val x = mkv ()
    (** Begin DEBUG edits *)
    val y = mkv ()	
    val coreOcc = (if coerceFnName = "finToInf" then
			FN(y, LT.ltc_int32 (** Where should this type come from *), 
			   APP(coreAcc coerceFnName, RECORD [VAR y,
				falseLexp 
				(** Apply to CoreBasicType falseDcon ...  *) ]))
		   else coreAcc coerceFnName) 
    (** End DEBUG edits *)
    val e = 	
    FN (x, orig_arg_lt,
	APP (PRIM (primop, new_lt, []),
	     RECORD [VAR x, coreAcc coerceFnName]))
     val _ = print ("### inl_infPrec ### coerceFnName " ^ coerceFnName ^ "\n")
    val _ = with_pp (fn ppstrm => PPLexp.ppLexp 20 ppstrm e)
    val _ = print "### end inl_infPrec ###\n" 
    in
    e	
end

(** transPrim : PrimOp.primop * Lty.lty * Lty.tyc list 
	
   Translate Absyn primop to PLambda form using given 
   intrinsic PLambda type and type parameters   
 *)
fun transPrim (prim, lt, ts) = 
  let fun g (PO.INLLSHIFT k) = inlineShift(lshiftOp, k, fn _ => lword0(k))
        | g (PO.INLRSHIFTL k) = inlineShift(rshiftlOp, k, fn _ => lword0(k))
        | g (PO.INLRSHIFT k) = (* preserve sign bit with arithmetic rshift *)
              let fun clear w = APP(PRIM(rshiftOp k, shiftTy k, []), 
                                    RECORD [w, WORD 0w31]) 
               in inlineShift(rshiftOp, k, clear)
              end

	| g (PO.INLMIN nk) = inlminmax (nk, false)
	| g (PO.INLMAX nk) = inlminmax (nk, true)
	| g (PO.INLABS nk) = inlabs nk

        | g (PO.INLNOT) =
              let val x = mkv()
               in FN(x, lt_bool, COND(VAR x, falseLexp, trueLexp))
              end 

        | g (PO.INLCOMPOSE) =
              let val (t1, t2, t3) = 
                    case ts of [a,b,c] => (lt_tyc a, lt_tyc b, lt_tyc c)
                             | _ => bug "unexpected type for INLCOMPOSE"

                  val argt = lt_tup [lt_arw(t2, t3), lt_arw(t1, t2)]

                  val x = mkv() and z = mkv() 
                  val f = mkv() and g = mkv()
               in FN(z, argt, 
                    LET(f, SELECT(0,VAR z),
                      LET(g,SELECT(1,VAR z),
                        FN(x, t1, APP(VAR f,APP(VAR g,VAR x))))))
              end                  
        | g (PO.INLBEFORE) =
              let val (t1, t2) = 
                    case ts of [a,b] => (lt_tyc a, lt_tyc b)
                             | _ => bug "unexpected type for INLBEFORE"
                  val argt = lt_tup [t1, t2]
                  val x = mkv()
               in FN(x, argt, SELECT(0,VAR x))
              end
	| g (PO.INLIGNORE) =
	  let val argt =
		  case ts of [a] => lt_tyc a
			   | _ => bug "unexpected type for INLIGNORE"
	  in FN (mkv (), argt, unitLexp)
	  end

	| g (PO.INLIDENTITY) =
	  let val argt =
		  case ts of [a] => lt_tyc a
			   | _ => bug "unexpected type for INLIDENTITY"
	      val v = mkv ()
	  in
	      FN (v, argt, VAR v)
	  end

	| g (PO.CVT64) = let val v = mkv () in FN (v, lt_i32pair, VAR v) end

        | g (PO.INLSUBSCRIPTV) =
              let val (tc1, t1) = case ts of [z] => (z, lt_tyc z)
                                    | _ => bug "unexpected ty for INLSUBV"

                  val seqtc = LT.tcc_vector tc1
                  val argt = lt_tup [lt_tyc seqtc, lt_int]

                  val oper = PRIM(PO.SUBSCRIPT, lt, ts)
                  val p = mkv() and a = mkv() and i = mkv()
                  val vp = VAR p and va = VAR a and vi = VAR i
               in FN(p, argt,
                    LET(a, SELECT(0,vp),
                      LET(i, SELECT(1,vp),
                        COND(APP(cmpOp(LESSU), 
                                 RECORD[vi, APP(lenOp seqtc, va)]),
                             APP(oper, RECORD[va, vi]),
                             mkRaise(coreExn "Subscript", t1)))))
              end

        | g (PO.INLSUBSCRIPT) = 
              let val (tc1, t1) = case ts of [z] => (z, lt_tyc z)
                                    | _ => bug "unexpected ty for INLSUB"

                  val seqtc = LT.tcc_array tc1
                  val argt = lt_tup [lt_tyc seqtc, lt_int]

                  val oper = PRIM(PO.SUBSCRIPT, lt, ts)
                  val p = mkv() and a = mkv() and i = mkv()
                  val vp = VAR p and va = VAR a and vi = VAR i
               in FN(p, argt,
                    LET(a, SELECT(0, vp),
                      LET(i, SELECT(1, vp),
                        COND(APP(cmpOp(LESSU), 
                                 RECORD[vi, APP(lenOp seqtc, va)]),
                             APP(oper, RECORD[va, vi]),
                             mkRaise(coreExn "Subscript", t1)))))
              end

        | g (PO.NUMSUBSCRIPT{kind,checked=true,immutable}) =
              let val (tc1, t1, t2) = 
                    case ts of [a,b] => (a, lt_tyc a, lt_tyc b)
                             | _ => bug "unexpected type for NUMSUB"

                  val argt = lt_tup [t1, lt_int]
                  val p = mkv() and a = mkv() and i = mkv()
                  val vp = VAR p and va = VAR a and vi = VAR i
                  val oper = PO.NUMSUBSCRIPT{kind=kind,checked=false,
                                             immutable=immutable}
                  val oper' = PRIM(oper, lt, ts)
               in FN(p, argt,
                    LET(a, SELECT(0, vp),
                      LET(i, SELECT(1, vp),
                        COND(APP(cmpOp(LESSU), RECORD[vi, 
                                                 APP(lenOp tc1, va)]),
                             APP(oper', RECORD [va, vi]),
                             mkRaise(coreExn "Subscript", t2)))))
              end

        | g (PO.INLUPDATE) = 
              let val (tc1, t1) = case ts of [z] => (z, lt_tyc z)
                                    | _ => bug "unexpected ty for INLSUB"

                  val seqtc = LT.tcc_array tc1
                  val argt = lt_tup [lt_tyc seqtc, lt_int, t1]

                  val oper = PRIM(PO.UPDATE, lt, ts)
                  val x = mkv() and a = mkv() and i = mkv() and v = mkv()
                  val vx = VAR x and va = VAR a and vi = VAR i and vv = VAR v

               in FN(x, argt,
                    LET(a, SELECT(0, vx),
                      LET(i, SELECT(1, vx),
                        LET(v, SELECT(2, vx),
                          COND(APP(cmpOp(LESSU),
                                   RECORD[vi,APP(lenOp seqtc, va)]),
                               APP(oper, RECORD[va,vi,vv]),
                               mkRaise(coreExn "Subscript", LT.ltc_unit))))))
              end

        | g (PO.NUMUPDATE{kind,checked=true}) =
              let val (tc1, t1, t2) = 
                    case ts of [a,b] => (a, lt_tyc a, lt_tyc b)
                             | _ => bug "unexpected type for NUMUPDATE"

                  val argt = lt_tup [t1, lt_int, t2]

                  val p=mkv() and a=mkv() and i=mkv() and v=mkv()
                  val vp=VAR p and va=VAR a and vi=VAR i and vv=VAR v

                  val oper = PO.NUMUPDATE{kind=kind,checked=false}
                  val oper' = PRIM(oper, lt, ts)
               in FN(p, argt,
                    LET(a, SELECT(0, vp),
                      LET(i, SELECT(1, vp),
                        LET(v, SELECT(2, vp),
                          COND(APP(cmpOp(LESSU),
                                   RECORD[vi,APP(lenOp tc1, va)]),
                               APP(oper', RECORD[va,vi,vv]),
                               mkRaise(coreExn "Subscript", LT.ltc_unit))))))
              end

(**** ASSIGN(r, x) <> UPDATE(r, 0, x) under new array reps (JHR;1998-10-30)
        | g (PO.ASSIGN) = 
              let val (tc1, t1) = case ts of [z] => (z, lt_tyc z)
                                    | _ => bug "unexpected ty for ASSIGN"

                  val seqtc = LT.tcc_ref tc1
                  val argt = lt_tup [lt_tyc seqtc, t1]

                  val oper = PRIM(PO.UPDATE, lt_upd, [tc1])

                  val x = mkv()
                  val varX = VAR x

               in FN(x, argt, 
                   APP(oper, RECORD[SELECT(0, varX), INT 0, SELECT(1, varX)]))
              end
****)

	(* Precision-conversion operations involving IntInf.
	 * These need to be translated specially by providing
	 * a second argument -- the routine from _Core that
	 * does the actual conversion to or from IntInf. *)

	| g (p as PO.TEST_INF prec) =
	    inlFromInfPrec ("TEST_INF", "testInf", p, lt)
	| g (p as PO.TRUNC_INF prec) =
	    inlFromInfPrec ("TRUNC_INF", "truncInf", p, lt)
	| g (p as PO.EXTEND_INF prec) =
	    (* inl_infPrec ("EXTEND_INF", "finToInf", p, lt, false) *)
	    inlToInfPrec("EXTEND_INF", "finToInf", p, lt)
	| g (p as PO.COPY_INF prec) =
	    inlToInfPrec ("COPY", "finToInf", p, lt) 
	(* default handling for all other primops *)
        | g p = PRIM(p, lt, ts) 

   in g prim
  end (* function transPrim *)

fun genintinfswitch (sv, cases, default) = let
    val v = mkv ()

    (* build a chain of equality tests for checking large pattern values *)
    fun build [] = default
      | build ((n, e) :: r) =
	  COND (APP (#getIntInfEq eqDict (), RECORD [VAR v, VAR (getII n)]),
		e, build r)

    (* split pattern values into small values and large values;
     * small values can be handled directly using SWITCH *)
    fun split ([], s, l) = (rev s, rev l)
      | split ((n, e) :: r, sm, lg) =
	  (case LN.lowVal n of
	       SOME l => split (r, (INTcon l, e) :: sm, lg)
	     | NONE => split (r, sm, (n, e) :: lg))

    fun gen () =
	case split (cases, [], []) of
	    ([], largeints) => build largeints
	  | (smallints, largeints) => let
		val iv = mkv ()
	    in
		LET (iv, APP (coreAcc "infLowValue", VAR v),
		     SWITCH (VAR iv,
			     DA.CNIL, smallints, SOME (build largeints)))
	    end
in
    LET (v, sv, gen ())
end
    

(***************************************************************************
 *                                                                         *
 * Translating various bindings into lambda expressions:                   *
 *                                                                         *
 *   val mkVar : V.var * DI.depth -> L.lexp                                *
 *   val mkVE : V.var * T.ty list -> L.lexp                                *
 *   val mkCE : T.datacon * T.ty list * L.lexp option * DI.depth -> L.lexp *
 *   val mkStr : M.Structure * DI.depth -> L.lexp                          *
 *   val mkFct : M.Functor * DI.depth -> L.lexp                            *
 *   val mkBnd : DI.depth -> B.binding -> L.lexp                           *
 *                                                                         *
 ***************************************************************************)
(* [KM???] mkVar is calling mkAccInfo, which just drops the prim!!! *)
fun mkVar (v as V.VALvar{access, prim, typ, path}, d) = 
      mkAccInfo(access, prim, fn () => toLty d (!typ), getNameOp path)
  | mkVar _ = bug "unexpected vars in mkVar"

(* mkVE : V.var * type list * depth -> lexp 
 * This translates a variable, which might be bound to a primop.
 * In the case of a primop variable, this function reconstructs the
 * type parameters of instantiation of the intrinsic primop type relative
 * to the variable occurrence type *)
fun mkVE (e as V.VALvar { typ, prim = PrimOpId.Prim p, ... }, ts, d) =
      let val occurenceTy = (* compute the occurrence type of the variable *)
              case ts
                of [] => !typ
                   (* ASSERT: !typ is not a POLYty *)
                 | _ => TU.applyPoly(!typ, ts)
	  val (primop,intrinsicType) =
              case (PrimOpMap.primopMap p, PrimOpTypeMap.primopTypeMap p)
               of (SOME p, SOME t) => (p,t)
                | _ => bug "mkVE: unrecognized primop name"
	  val _ = debugmsg ">>mkVE: before matchInstTypes"
	  val intrinsicParams =
              (* compute intrinsic instantiation params of intrinsicType *)
              case (TU.matchInstTypes(true, occurenceTy, intrinsicType)
                      : (TP.tyvar list * TP.tyvar list) option )
                of SOME(_, tvs) => 
		   (if !debugging then
                      complain EM.WARN
                        "mkVE ->matchInstTypes -> pruneTyvar"
                        (fn ppstrm => 
                          (PP.string ppstrm
                            ("tvs length: " ^ Int.toString (length tvs));
                           PP.newline ppstrm;
                           PPVal.ppDebugVar
                            (fn x => "") ppstrm env e;
                           if (length tvs) = 1
                           then PPType.ppType env ppstrm (TP.VARty (hd tvs))
                           else ()))
                    else ();
                    map TU.pruneTyvar tvs)
                 | NONE =>
                   (ElabDebug.withInternals (fn () => (complain EM.COMPLAIN
                      "mkVE:primop intrinsic type doesn't match occurrence type"
                      (fn ppstrm => 
                          (PP.string ppstrm "VALvar: ";
                           PPVal.ppVar ppstrm e;
                           PP.newline ppstrm;
                           PP.string ppstrm "occtypes: ";
                           PPType.ppType env ppstrm occurenceTy;
                           PP.newline ppstrm;
                           PP.string ppstrm "intrinsicType: ";
                           PPType.ppType env ppstrm intrinsicType;
                           PP.newline ppstrm;
                           PP.string ppstrm "instpoly occ: ";
                           PPType.ppType env ppstrm
                             (#1 (TU.instantiatePoly occurenceTy));
                           PP.newline ppstrm;
                           PP.string ppstrm "instpoly intrinsicType: ";
                           PPType.ppType env ppstrm
                             (#1 (TU.instantiatePoly intrinsicType))));
                    bug "mkVE -- NONE")))
	  val _ = debugmsg "<<mkVE: after matchInstTypes"
       in case (primop, intrinsicParams)
            of (PO.POLYEQL, [t]) => eqGen(intrinsicType, t, toTcLt d)
             | (PO.POLYNEQ, [t]) =>
               composeNOT(eqGen(intrinsicType, t, toTcLt d), toLty d t)
             | (PO.INLMKARRAY, [t]) => 
               let val dict = 
                       {default = coreAcc "mkNormArray",
                        table = [([LT.tcc_real], coreAcc "mkRealArray")]}
                in GENOP (dict, primop, toLty d intrinsicType,
                         map (toTyc d) intrinsicParams)
               end
             | (PO.RAW_CCALL NONE, [a, b, c]) =>
               let val i = SOME (CProto.decode cproto_conv
                                   { fun_ty = a, encoding = b })
                           handle CProto.BadEncoding => NONE
               in PRIM (PO.RAW_CCALL i, toLty d intrinsicType,
                        map (toTyc d) intrinsicParams)
               end
             | _ => (** where do these intrinsicType originate? 
			A: PrimOpTypeMap *)
		    transPrim(primop, (toLty d intrinsicType),
                              map (toTyc d) intrinsicParams)
      end
  | mkVE (v as V.VALvar{typ, prim = PrimOpId.NonPrim, path, ...}, ts, d) =
    (* non primop variable *)
      (if !debugging
       then (print "### mkVE nonprimop\n";
             print (SymPath.toString path); print "\n";
             ppType (!typ); print "\n";
             print "|ts| = "; print (Int.toString(length ts)); print "\n";
             app ppType ts; print "\n")
       else ();
       case ts
         of [] => mkVar (v, d)
          | _ => TAPP(mkVar(v, d), map (toTyc d) ts))
                 (* dbm: when does this second case occur? *)
  | mkVE _ = bug "non VALvar passed to mkVE"


fun mkCE (TP.DATACON{const, rep, name, typ, ...}, ts, apOp, d) = 
  let val lt = toDconLty d typ
      val rep' = mkRep(rep, lt, name)
      val dc = (name, rep', lt)
      val ts' = map (toTyc d o TP.VARty) ts
   in if const then CON'(dc, ts', unitLexp)
      else (case apOp
             of SOME le => CON'(dc, ts', le)
              | NONE => 
                 let val (argT, _) = LT.ltd_parrow(LT.lt_pinst(lt, ts'))
                     val v = mkv()
                  in FN(v, argT, CON'(dc, ts', VAR v))
                 end)
  end 

fun mkStr (s as M.STR { access, prim, ... }, d) =
    mkAccInfo(access, prim, fn () => strLty(s, d, compInfo), NONE)
  | mkStr _ = bug "unexpected structures in mkStr"

fun mkFct (f as M.FCT { access, prim, ... }, d) =
    mkAccInfo(access, prim, fn () => fctLty(f, d, compInfo), NONE)
  | mkFct _ = bug "unexpected functors in mkFct"

fun mkBnd d =
  let fun g (B.VALbind v) = mkVar(v, d)
        | g (B.STRbind s) = mkStr(s, d)
        | g (B.FCTbind f) = mkFct(f, d)
        | g (B.CONbind (TP.DATACON{rep=(DA.EXN acc), name, typ, ...})) =
          let val nt = toDconLty d typ
              val (argt,_) = LT.ltd_parrow nt
          in mkAccT (acc, LT.ltc_etag argt, SOME name)
          end
        | g _ = bug "unexpected bindings in mkBnd"
   in g
  end


(***************************************************************************
 *                                                                         *
 * Translating core absyn declarations into lambda expressions:            *
 *                                                                         *
 *    val mkVBs  : Absyn.vb list * depth -> PLambda.lexp -> PLambda.lexp     *
 *    val mkRVBs : Absyn.rvb list * depth -> PLambda.lexp -> PLambda.lexp    *
 *    val mkEBs  : Absyn.eb list * depth -> PLambda.lexp -> PLambda.lexp     *
 *                                                                         *
 ***************************************************************************)

(* mkPE : Absyn.exp * depth * Types.tyvar list -> PLambda.lexp
 * translate an expression with potential type parameters *)
fun mkPE (exp, d, []) = mkExp(exp, d)
  | mkPE (exp, d, boundtvs) = 
      let val savedtvs = map ! boundtvs
            (* save original contents of boundtvs for later restoration
             * by the restore function below *)

          fun setbtvs (i, []) = ()
            | setbtvs (i, (tv as ref (TP.OPEN _))::rest) =
		let val m = markLBOUND (d, i)
	         in tv := TP.TV_MARK (d,i);
		    setbtvs (i+1, rest)
	        end
            | setbtvs (i, (tv as ref (TP.TV_MARK _))::res) =
                bug ("unexpected tyvar TV_MARK in mkPE")
            | setbtvs _ = bug "unexpected tyvar INSTANTIATED in mkPE"

          val _ = setbtvs(0, boundtvs)
            (* assign TV_MARKs to the boundtvs to mark them as type
             * parameter variables during translation of exp *)

          val exp' = mkExp(exp, DI.next d)
            (* increase the depth to indicate that the expression is
             * going to be wrapped by a type abstraction (TFN); see body *)

          (* restore tyvar states to that before the translation *)
          fun restore ([], []) = ()
            | restore (a::r, b::z) = (b := a; restore(r, z))
            | restore _ = bug "unexpected cases in mkPE"

          (* [dbm, 6/22/06] Why do we need to restore the original
             contents of the uninstantiated meta type variables? 
             Only seems to be necessary if a given tyvar gets generalized
             in two different valbinds. We assume that this does not
             happen (Single Generalization Conjecture) *)

          val _ = restore(savedtvs, boundtvs)
          val len = length(boundtvs)
       
       in TFN(LT.tkc_arg(len), exp')
      end

and mkVBs (vbs, d) =
  let fun mkVB (VB{pat=VARpat(V.VALvar{access=DA.LVAR v, ...}),
                   exp as VARexp (ref (w as (V.VALvar{typ,prim,...})), ptvs),
                   boundtvs=btvs, ...}, b: lexp) = 
            (* [dbm: 7/10/06] Originally, the mkVar and mkPE translations
             * were chosen based on whether btvs and ptvs were the same
             * list of tyvars, which would be the case for all non-primop
             * variables, but also in the primop case whenever the rhs
             * variable environment type (!typ) was the same (equalTypeP)
             * to the intrinsic type of the primop (e.g. when they are
             * both monotypes).  So in most cases, the mkVar translation
             * will be used, and this drops the primop information!!!
             * This seems definitely wrong. *)
            	(case prim
              of PrimOpId.Prim name =>
                  (case PrimOpTypeMap.primopTypeMap name
                     of SOME(primopty) =>
                        (case TU.matchInstTypes(true,TU.applyPoly(!typ,
				map (TU.prune o TP.VARty) ptvs),primopty)
                        of NONE => bug "mkVB: occtype and intrinsic don't match"
			 | SOME(_,[]) => LET(v, mkVar(w, d), b)
                         | SOME(_,ptvs') => LET(v, mkPE(exp, d, btvs), b))
                      | NONE => bug "mkVBs: unknown primop name")
               | _ => LET(v, mkPE(exp, d, btvs), b))
(*
               | _ => LET(v, mkVar(w, d), b))
*)
                 (* when generalized variables = instantiation params *)

        | mkVB (VB{pat=VARpat(V.VALvar{access=DA.LVAR v, ...}),
                   exp, boundtvs=btvs, ...}, b) =
            LET(v, mkPE(exp, d, btvs), b)

        | mkVB (VB{pat=CONSTRAINTpat(VARpat(V.VALvar{access=DA.LVAR v, ...}),_),
                   exp, boundtvs=tvs, ...}, b) =
            LET(v, mkPE(exp, d, tvs), b)

        | mkVB (VB{pat, exp, boundtvs=tvs, ...}, b) =
            let val ee = mkPE(exp, d, tvs)
                val rules = [(fillPat(pat, d), b), (WILDpat, unitLexp)]
                val rootv = mkv()
                fun finish x = LET(rootv, ee, x)
             in MC.bindCompile(env, rules, finish, rootv, toTcLt d, complain,
			       genintinfswitch)
            end

   in fold mkVB vbs
  end (* mkVBs *)

and mkRVBs (rvbs, d) =
  let fun mkRVB (RVB{var=V.VALvar{access=DA.LVAR v, typ=ref ty, ...},
                     exp, boundtvs=btvs, ...}, (vlist, tlist, elist)) = 
            let val ee = mkExp(exp, d) (* was mkPE(exp, d, btvs) *)
                (* [ZHONG?] we no longer track type bindings at RVB anymore ! *)
                val vt = toLty d ty
            in (v::vlist, vt::tlist, ee::elist)
            end
        | mkRVB _ = bug "unexpected valrec bindings in mkRVBs"

      val (vlist, tlist, elist) = foldr mkRVB ([], [], []) rvbs

   in fn b => FIX(vlist, tlist, elist, b)
  end

and mkEBs (ebs, d) = 
  let fun g (EBgen {exn=TP.DATACON{rep=DA.EXN(DA.LVAR v), typ, ...}, 
                    ident, ...}, b) =
              let val nt = toDconLty d typ
                  val (argt, _) = LT.ltd_parrow nt
               in LET(v, ETAG(mkExp(ident, d), argt), b)
              end
        | g (EBdef {exn=TP.DATACON{rep=DA.EXN(DA.LVAR v), typ, name, ...},
                    edef=TP.DATACON{rep=DA.EXN(acc), ...}}, b) =
              let val nt = toDconLty d typ
                  val (argt, _) = LT.ltd_parrow nt
               in LET(v, mkAccT(acc, LT.ltc_etag argt, SOME name), b)
              end
        | g _ = bug "unexpected exn bindings in mkEBs"

   in fold g ebs
  end


(***************************************************************************
 *                                                                         *
 * Translating module exprs and decls into lambda expressions:             *
 *                                                                         *
 *    val mkStrexp : Absyn.strexp * depth -> PLambda.lexp                   *
 *    val mkFctexp : Absyn.fctexp * depth -> PLambda.lexp                   *
 *    val mkStrbs  : Absyn.strb list * depth -> PLambda.lexp -> PLambda.lexp *
 *    val mkFctbs  : Absyn.fctb list * depth -> PLambda.lexp -> PLambda.lexp *
 *                                                                         *
 ***************************************************************************)
and mkStrexp (se, d) = 
  let fun g (VARstr s) = mkStr(s, d)
        | g (STRstr bs) = SRECORD (map (mkBnd d) bs)
        | g (APPstr {oper, arg, argtycs}) = 
              let val e1 = mkFct(oper, d)
                  val tycs = map (tpsTyc d) argtycs
                  val e2 = mkStr(arg, d)
               in APP(TAPP(e1, tycs), e2)
              end
        | g (LETstr (dec, b)) = mkDec (dec, d) (g b)
        | g (MARKstr (b, reg)) = withRegion reg g b

   in g se
  end

and mkFctexp (fe, d) = 
  let fun g (VARfct f) = mkFct(f, d)
        | g (FCTfct {param as M.STR { access, ... }, argtycs, def }) =
	  (case access of
	       DA.LVAR v =>
               let val knds = map tpsKnd argtycs
                   val nd = DI.next d  (* reflecting type abstraction *)
                   val body = mkStrexp (def, nd)
                   val hdr = buildHdr v
               (* binding of all v's components *)
               in
		   TFN(knds, FN(v, strLty(param, nd, compInfo), hdr body))
               end
	     | _ => bug "mkFctexp: unexpected access")
        | g (LETfct (dec, b)) = mkDec (dec, d) (g b)
        | g (MARKfct (b, reg)) = withRegion reg g b
        | g _ = bug "unexpected functor expressions in mkFctexp"

   in g fe
  end

and mkStrbs (sbs, d) =
  let fun g (STRB{str=M.STR { access, ... }, def, ... }, b) =
	  (case access of
	       DA.LVAR v =>
               let val hdr = buildHdr v 
               (* binding of all v's components *)
               in
		   LET(v, mkStrexp(def, d), hdr b)
               end
	     | _ => bug "mkStrbs: unexpected access")
        | g _ = bug "unexpected structure bindings in mkStrbs"
  in fold g sbs
  end

and mkFctbs (fbs, d) = 
  let fun g (FCTB{fct=M.FCT { access, ... }, def, ... }, b) =
	  (case access of
	       DA.LVAR v =>
               let val hdr = buildHdr v
               in
		   LET(v, mkFctexp(def, d), hdr b)
               end
	     | _ => bug "mkFctbs: unexpected access")
        | g _ = bug "unexpected functor bindings in mkStrbs"
  in fold g fbs
  end


(***************************************************************************
 * Translating absyn decls and exprs into lambda expression:               *
 *                                                                         *
 *    val mkExp : A.exp * DI.depth -> PLambda.lexp                         *
 *    val mkDec : A.dec * DI.depth -> PLambda.lexp -> PLambda.lexp         *
 *                                                                         *
 ***************************************************************************)
and mkDec (dec, d) = 
  let fun g (VALdec vbs) = mkVBs(vbs, d)
        | g (VALRECdec rvbs) = mkRVBs(rvbs, d)
        | g (ABSTYPEdec{body,...}) = g body
        | g (EXCEPTIONdec ebs) = mkEBs(ebs, d)
        | g (STRdec sbs) = mkStrbs(sbs, d)
        | g (ABSdec sbs) = mkStrbs(sbs, d)
        | g (FCTdec fbs) = mkFctbs(fbs, d)
        | g (LOCALdec(ld, vd)) = (g ld) o (g vd)
        | g (SEQdec ds) =  foldr (op o) ident (map g ds)
        | g (MARKdec(x, reg)) = 
              let val f = withRegion reg g x
               in fn y => withRegion reg f y
              end
        | g (OPENdec xs) = 
              let (* special hack to make the import tree simpler *)
                  fun mkos (_, s as M.STR { access = acc, ... }) =
                      if extern acc then 
                          let val _ = mkAccT(acc, strLty(s, d, compInfo), NONE)
                          in ()
                          end
                      else ()
                    | mkos _ = ()
               in app mkos xs; ident
              end
        | g _ = ident
   in g dec
  end

and mkExp (exp, d) = 
  let val tTyc = toTyc d
      val tLty = toLty d

      fun mkRules xs = map (fn (RULE(p, e)) => (fillPat(p, d), g e)) xs

      and g (VARexp (ref v, ts)) = 
            (debugmsg ">>mkExp VARexp"; 
	     mkVE(v, map TP.VARty ts, d))
        | g (CONexp (dc, ts)) = 
	  (let val _ = debugmsg ">>mkExp CONexp: "
	       val c = mkCE(dc, ts, NONE, d)
	       val _ = if !debugging then ppLexp c else ()
	   in c end)
        | g (APPexp (CONexp(dc, ts), e2)) = 
	  (let val _ = debugmsg ">>mkExp APPexp: "
	       val c = mkCE(dc, ts, SOME(g e2), d)
	       val _ = if !debugging then ppLexp c else ()
	   in c end)
        | g (INTexp (s, t)) =
	  (debugmsg ">>mkExp INTexp";
             ((if TU.equalType (t, BT.intTy) then INT (LN.int s)
               else if TU.equalType (t, BT.int32Ty) then INT32 (LN.int32 s)
	       else if TU.equalType (t, BT.intinfTy) then VAR (getII s)
	       else if TU.equalType (t, BT.int64Ty) then
		   let val (hi, lo) = LN.int64 s
		   in RECORD [WORD32 hi, WORD32 lo]
		   end
               else bug "translate INTexp")
              handle Overflow => (repErr "int constant too large"; INT 0)))

        | g (WORDexp(s, t)) =
	  (debugmsg ">>WORDexp";
             ((if TU.equalType (t, BT.wordTy) then WORD (LN.word s)
               else if TU.equalType (t, BT.word8Ty) then WORD (LN.word8 s)
               else if TU.equalType (t, BT.word32Ty) then WORD32 (LN.word32 s)
	       else if TU.equalType (t, BT.word64Ty) then
		   let val (hi, lo) = LN.word64 s
		   in RECORD [WORD32 hi, WORD32 lo]
		   end
               else (ppType t; bug "translate WORDexp"))
               handle Overflow => (repErr "word constant too large"; INT 0)))

        | g (REALexp s) = REAL s
        | g (STRINGexp s) = STRING s
        | g (CHARexp s) = INT (Char.ord(String.sub(s, 0)))
             (** NOTE: the above won't work for cross compiling to 
                       multi-byte characters **)

        | g (RECORDexp []) = unitLexp
        | g (RECORDexp xs) =
             if sorted xs then RECORD (map (fn (_,e) => g e) xs)
             else let val vars = map (fn (l,e) => (l,(g e, mkv()))) xs
                      fun bind ((_,(e,v)),x) = LET(v,e,x)
                      val bexp = map (fn (_,(_,v)) => VAR v) (sortrec vars)
                   in foldr bind (RECORD bexp) vars
                  end

        | g (SELECTexp (LABEL{number=i,...}, e)) = SELECT(i, g e)

        | g (VECTORexp ([], ty)) = 
             TAPP(coreAcc "vector0", [tTyc ty])
        | g (VECTORexp (xs, ty)) = 
             let val tc = tTyc ty
                 val vars = map (fn e => (g e, mkv())) xs
                 fun bind ((e,v),x) = LET(v, e, x)
                 val bexp = map (fn (_,v) => VAR v) vars
              in foldr bind (VECTOR (bexp, tc)) vars
             end 

        | g (PACKexp(e, ty, tycs)) = g e
(* [dbm, 7/10/06]: Does PACKexp do anything now? What was it doing before
 * this was commented out? This appears to be the only place reformat was called
 * Is it also the only place the FLINT PACK constructor is used? [KM???] *)
(* (commented out by whom, when why?)
             let val (nty, ks, tps) = TU.reformat(ty, tycs, d)
                 val ts = map (tpsTyc d) tps
                 (** use of LtyEnv.tcAbs is a temporary hack (ZHONG) **)
                 val nts = ListPair.map LtyEnv.tcAbs (ts, ks)
                 val nd = DI.next d
              in case (ks, tps)
                  of ([], []) => g e
                   | _ => PACK(LT.ltc_poly(ks, [toLty nd nty]), 
                               ts, nts , g e)
             end
*)
        | g (SEQexp [e]) = g e
        | g (SEQexp (e::r)) = LET(mkv(), g e, g (SEQexp r)) 

        | g (APPexp (e1, e2)) = APP(g e1, g e2)
        | g (MARKexp (e, reg)) = withRegion reg g e
        | g (CONSTRAINTexp (e,_)) = g e

        | g (RAISEexp (e, ty)) = mkRaise(g e, tLty ty)
        | g (HANDLEexp (e, (l, ty))) =
             let val rootv = mkv()
                 fun f x = FN(rootv, tLty ty, x)
                 val l' = mkRules l
              in HANDLE(g e, MC.handCompile(env, l', f, 
                                            rootv, toTcLt d, complain,
					    genintinfswitch))
             end

        | g (FNexp (l, ty)) = 
             let val rootv = mkv()
                 fun f x = FN(rootv, tLty ty, x)
              in MC.matchCompile (env, mkRules l, f, rootv, toTcLt d,
				  complain, genintinfswitch)
             end

        | g (CASEexp (ee, l, isMatch)) = 
             let val rootv = mkv()
                 val ee' = g ee
                 fun f x = LET(rootv, ee', x)
                 val l' = mkRules l
              in if isMatch 
                 then MC.matchCompile (env, l', f, rootv, toTcLt d,
				       complain, genintinfswitch)
                 else MC.bindCompile (env, l', f, rootv, toTcLt d,
				      complain, genintinfswitch)
             end

	| g (IFexp { test, thenCase, elseCase }) =
	    COND (g test, g thenCase, g elseCase)

	| g (ANDALSOexp (e1, e2)) =
	    COND (g e1, g e2, falseLexp)

	| g (ORELSEexp (e1, e2)) =
	    COND (g e1, trueLexp, g e2)

	| g (WHILEexp { test, expr }) =
	    let val fv = mkv ()
		val body =
		    FN (mkv (), lt_unit,
			COND (g test,
			      LET (mkv (), g expr, APP (VAR fv, unitLexp)),
			      unitLexp))
	    in
		FIX ([fv], [lt_u_u], [body], APP (VAR fv, unitLexp))
	    end

        | g (LETexp (dc, e)) = mkDec (dc, d) (g e)

        | g e = 
             EM.impossibleWithBody "untranslateable expression"
              (fn ppstrm => (PP.string ppstrm " expression: ";
                            PPAbsyn.ppExp (env,NONE) ppstrm (e, !ppDepth)))

   in g exp
  end 

and transIntInf d s =
    (* This is a temporary solution.  Since IntInf literals
     * are created using a core function call, there is
     * no indication within the program that we are really
     * dealing with a constant value that -- in principle --
     * could be subject to such things as constant folding. *)
    let val consexp = CONexp (BT.consDcon, [ref (TP.INSTANTIATED BT.wordTy)])
	fun build [] = CONexp (BT.nilDcon, [ref (TP.INSTANTIATED BT.wordTy)])
	  | build (d :: ds) = let
		val i = Word.toIntX d
	    in
		APPexp (consexp,
			EU.TUPLEexp [WORDexp (IntInf.fromInt i, BT.wordTy),
				     build ds])
	    end
	fun small w =
	    APP (coreAcc (if LN.isNegative s then "makeSmallNegInf"
			  else "makeSmallPosInf"),
		 mkExp (WORDexp (IntInf.fromInt (Word.toIntX w), BT.wordTy),
			d))
    in
	case LN.repDigits s of
	    [] => small 0w0
	  | [w] => small w
	  | ws => APP (coreAcc (if LN.isNegative s then "makeNegInf"
				else "makePosInf"),
		       mkExp (build ws, d))
    end

(* Wrap bindings for IntInf.int literals around body. *)
fun wrapII body = let
    fun one (n, v, b) = LET (v, transIntInf DI.top n, b)
in
    IIMap.foldli one body (!iimap)
end

(* wrapPidInfo: lexp * (pid * pidInfo) list -> lexp * importTree *)
fun wrapPidInfo (body, pidinfos) = 
  let val imports = 
        let fun p2itree (ANON xl) = 
                  ImportTree.ITNODE (map (fn (i,z) => (i, p2itree z)) xl)
              | p2itree (NAMED _) = ImportTree.ITNODE []
         in map (fn (p, pi) => (p, p2itree pi)) pidinfos
        end
(*
      val _ = let val _ = say "\n ****************** \n"
                  val _ = say "\n the current import tree is :\n"
                  fun tree (ImportTree.ITNODE []) = ["\n"]
                    | tree (ImportTree.ITNODE xl) = 
                        foldr (fn ((i, x), z) => 
                          let val ts = tree x
                              val u = (Int.toString i)  ^ "   "
                           in (map (fn y => (u ^ y)) ts) @ z
                          end) [] xl
                  fun pp (p, n) = 
                    (say ("Pid " ^ (PersStamps.toHex p) ^ "\n"); 
                     app say (tree n))
               in app pp imports; say "\n ****************** \n"
              end
*)
      val plexp = 
        let fun get ((_, ANON xl), z) = foldl get z xl
              | get ((_, u as NAMED (_,t,_)), (n,cs,ts)) = 
                  (n+1, (n,u)::cs, t::ts)

            (* get the fringe information *)
            val getp = fn ((_, pi), z) => get((0, pi), z) 
            val (finfos, lts) = 
              let val (_, fx, lx) = foldl getp (0,[],[]) pidinfos
               in (rev fx, rev lx)
              end

            (* do the selection of all import variables *)
            fun mksel (u, xl, be) = 
              let fun g ((i, pi), be) = 
                    let val (v, xs) = case pi of ANON z => (mkv(), z)
                                               | NAMED(v,_,z) => (v, z)
                     in LET(v, SELECT(i, u), mksel(VAR v, xs, be))
                    end
               in foldr g be xl
              end
            val impvar = mkv()
            val implty = LT.ltc_str lts
            val nbody = mksel (VAR impvar, finfos, body) 
         in FN(impvar, implty, nbody)
        end
   in (plexp, imports)
  end (* function wrapPidInfo *)

(** the list of things being exported from the current compilation unit *)
val exportLexp = SRECORD (map VAR exportLvars)

val _ = debugmsg ">>mkDec"
(** translating the ML absyn into the PLambda expression *)
val body = mkDec (rootdec, DI.top) exportLexp
val _ = debugmsg "<<mkDec"
(** add bindings for intinf constants *)
val body = wrapII body

(** wrapping up the body with the imported variables *)
val (plexp, imports) = wrapPidInfo (body, PersMap.listItemsi (!persmap))

(** type check body (including kind check) **)
val _ = complain EM.WARN ">>translate typecheck" EM.nullErrorBody
val _ = print "**** Translate: typechecking plexp ****\n"
(* val _ = with_pp(fn strm => PPLexp.ppLexp 20 strm plexp) *)
val ltyerrors = ChkPlexp.checkLtyTop(plexp,0)
val _ = if ltyerrors
        then (print "**** Translate: checkLty failed ****\n";
              with_pp(fn str =>
                (PU.pps str "absyn:"; PP.newline str;
                 ElabDebug.withInternals
                  (fn () => PPAbsyn.ppDec (env,NONE) str (rootdec,1000)); PP.newline str;
                 PU.pps str "lexp:"; PP.newline str;
                 PPLexp.ppLexp 25 str plexp));
              complain EM.WARN "checkLty" EM.nullErrorBody;
	     bug "PLambda type check error!")
        else print "**** Translate: finished typechecking plexp ****\n"


fun prGen (flag,printE) s e =
  if !flag then (say ("\n\n[After " ^ s ^ " ...]\n\n"); printE e) else ()
val _ = prGen(Control.FLINT.print, ppLexp) "Translate" plexp

(** normalizing the plambda expression into FLINT *)
val flint = let val _ = debugmsg ">>norm"
		val _ = if !debugging 
			then complain EM.WARN ">>flintnm" EM.nullErrorBody
			else ()
		val n = FlintNM.norm plexp
		val _ = debugmsg "<<postnorm"
	    in n end

in {flint = flint, imports = imports}
end (* function transDec *)

end (* top-level local *)
end (* structure Translate *)
