(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* translate.sml *)

signature TRANSLATE = 
sig

  (* Invariant: transDec always applies to a top-level absyn declaration *) 
  val transDec : Absyn.dec * Access.lvar list 
                 * StaticEnv.staticEnv * CompInfo.compInfo
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
      structure II = InlInfo
      structure LT = PLambdaType
      structure M  = Modules
      structure MC = MatchComp
      structure PO = PrimOp
      structure PP = PrettyPrint
      structure S  = Symbol
      structure SP = SymPath
      structure LN = LiteralToNum
      structure TT = TransTypes
      structure TP = Types
      structure TU = TypesUtil
      structure V  = VarCon

      structure Map = PersMap

      open Absyn PLambda 
in 

(****************************************************************************
 *                   CONSTANTS AND UTILITY FUNCTIONS                        *
 ****************************************************************************)

val debugging = ref true
fun bug msg = EM.impossible("Translate: " ^ msg)
val say = Control.Print.say
val ppDepth = Control.Print.printDepth

fun ppType ty =
    ElabDebug.withInternals
     (fn () => ElabDebug.debugPrint debugging
		("type: ",PPType.ppType StaticEnv.empty, ty))

fun ident x = x
val unitLexp = RECORD []

fun getNameOp p = if SP.null p then NONE else SOME(SP.last p)

type pid = PersStamps.persstamp

(** old-style fold for cases where it is partially applied *)
fun fold f l init = foldr f init l

(*
 * MAJOR CLEANUP REQUIRED ! The function mkv is currently directly taken 
 * from the LambdaVar module; I think it should be taken from the 
 * "compInfo". Similarly, should we replace all mkLvar in the backend
 * with the mkv in "compInfo" ? (ZHONG)
 *)
val mkv = LambdaVar.mkLvar 
fun mkvN NONE = mkv()
  | mkvN (SOME s) = LambdaVar.namedLvar s

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

fun transDec (rootdec, exportLvars, env,
	      compInfo as {errorMatch,error,...}: CompInfo.compInfo) =
let 

(** generate the set of ML-to-FLINT type translation functions *)
val {tpsKnd, tpsTyc, toTyc, toLty, strLty, fctLty} = TT.genTT()
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
val persmap = ref (Map.empty : pidInfo Map.map)

fun mkPid (pid, t, l, nameOp) =
    case Map.find (!persmap, pid)
      of NONE => 
	  let val (pinfo, var) = mkPidInfo (t, l, nameOp)
	   in persmap := Map.insert(!persmap, pid, pinfo);
	      var
	  end
       | SOME pinfo =>
	  let val (npinfo, var) = mergePidInfo (pinfo, t, l, nameOp)
	      fun rmv (key, map) = 
		  let val (newMap, _) = Map.remove(map, key) 
		  in newMap
		  end handle e => map
	   in persmap := Map.insert(rmv(pid, !persmap), pid, npinfo);
	      var
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
fun coreExn id =
  ((case coreLookup(id, env)
     of V.CON(TP.DATACON{name, rep as DA.EXN _, typ, ...}) => 
          let val nt = toDconLty DI.top typ
              val nrep = mkRep(rep, nt, name)
           in CON'((name, nrep, nt), [], unitLexp)
          end
      | _ => bug "coreExn in translate")
   handle NoCore => (say "WARNING: no Core access \n"; INT 0))

and coreAcc id =
  ((case coreLookup(id, env)
     of V.VAL(V.VALvar{access, typ, path, ...}) => 
           mkAccT(access, toLty DI.top (!typ), getNameOp path)
      | _ => bug "coreAcc in translate")
   handle NoCore => (say "WARNING: no Core access \n"; INT 0))


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

(** converting a value of access+info into the lambda expression *)
fun mkAccInfo (acc, info, getLty, nameOp) = 
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
                                    (PP.add_newline ppstrm;
                                     PP.add_string ppstrm "pattern: ";
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
        | fill (CONpat(TP.DATACON{name, const, typ, rep, sign, lazyp}, ts)) = 
            CONpat(TP.DATACON{name=name, const=const, typ=typ, lazyp=lazyp,
                        sign=sign, rep=mkRep(rep, toDconLty d typ, name)}, ts)
        | fill (APPpat(TP.DATACON{name, const, typ, rep, sign, lazyp}, ts, pat)) = 
            APPpat(TP.DATACON{name=name, const=const, typ=typ, sign=sign, lazyp=lazyp,
                       rep=mkRep(rep, toDconLty d typ, name)}, ts, fill pat)
        | fill xp = xp

   in fill pat
  end (* function fillPat *)

(** The runtime polymorphic equality and string equality dictionary. *)
val eqDict =
  let val strEqRef : lexp option ref = ref NONE
      val polyEqRef : lexp option ref = ref NONE

      fun getStrEq () = 
        (case (!strEqRef) 
          of SOME e => e
           | NONE => (let val e = coreAcc "stringequal"
                       in strEqRef := (SOME e); e
                      end))

      fun getPolyEq () = 
        (repPolyEq();
	 case (!polyEqRef) 
          of SOME e => e
           | NONE => (let val e = coreAcc "polyequal"
                       in polyEqRef := (SOME e); e
                      end))
   in {getStrEq=getStrEq, getPolyEq=getPolyEq}
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

val lt_ipair = lt_tup [lt_int, lt_int]
val lt_icmp = lt_arw (lt_ipair, lt_bool)
val lt_ineg = lt_arw (lt_int, lt_int)
val lt_intop = lt_arw (lt_ipair, lt_int)

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

fun intOp p = PRIM(p, lt_intop, [])
fun cmpOp p = PRIM(p, lt_icmp, [])
fun inegOp p = PRIM(p, lt_ineg, [])

fun ADD(b,c) = APP(intOp(PO.IADD), RECORD[b, c])
fun SUB(b,c) = APP(intOp(PO.ISUB), RECORD[b, c])
fun MUL(b,c) = APP(intOp(PO.IMUL), RECORD[b, c])
fun DIV(b,c) = APP(intOp(PO.IDIV), RECORD[b, c])
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
  let fun shiftLimit (PO.UINT lim) = WORD(Word.fromInt lim)
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


fun transPrim (prim, lt, ts) = 
  let fun g (PO.INLLSHIFT k) = inlineShift(lshiftOp, k, fn _ => lword0(k))
        | g (PO.INLRSHIFTL k) = inlineShift(rshiftlOp, k, fn _ => lword0(k))
        | g (PO.INLRSHIFT k) = (* preserve sign bit with arithmetic rshift *)
              let fun clear w = APP(PRIM(rshiftOp k, shiftTy k, []), 
                                    RECORD [w, WORD 0w31]) 
               in inlineShift(rshiftOp, k, clear)
              end

        | g (PO.INLDIV) =  
              let val a = mkv() and b = mkv() and z = mkv()
               in FN(z, lt_ipair, 
                    LET(a, SELECT(0, VAR z),
                      LET(b, SELECT(1, VAR z),
                        COND(APP(cmpOp(PO.IGE), RECORD[VAR b, INT 0]),
                          COND(APP(cmpOp(PO.IGE), RECORD[VAR a, INT 0]),
                               DIV(VAR a, VAR b),
                               SUB(DIV(ADD(VAR a, INT 1), VAR b), INT 1)),
                          COND(APP(cmpOp(PO.IGT), RECORD[VAR a, INT 0]),
                               SUB(DIV(SUB(VAR a, INT 1), VAR b), INT 1),
                               DIV(VAR a, VAR b))))))
              end 

        | g (PO.INLMOD) =
              let val a = mkv() and b = mkv() and z = mkv()
               in FN(z, lt_ipair,
                    LET(a,SELECT(0, VAR z),
                      LET(b,SELECT(1,VAR z),
                        COND(APP(cmpOp(PO.IGE), RECORD[VAR b, INT 0]),
                          COND(APP(cmpOp(PO.IGE), RECORD[VAR a, INT 0]),
                               SUB(VAR a, MUL(DIV(VAR a, VAR b), VAR b)),
                               ADD(SUB(VAR a,MUL(DIV(ADD(VAR a,INT 1), VAR b),
                                                 VAR b)), VAR b)),
                          COND(APP(cmpOp(PO.IGT), RECORD[VAR a,INT 0]),
                               ADD(SUB(VAR a,MUL(DIV(SUB(VAR a,INT 1), VAR b),
                                                 VAR b)), VAR b),
                               COND(APP(cmpOp(PO.IEQL),RECORD[VAR a,
                                                         INT ~1073741824]),
                                    COND(APP(cmpOp(PO.IEQL),
                                             RECORD[VAR b,INT 0]), 
                                         INT 0,
                                         SUB(VAR a, MUL(DIV(VAR a, VAR b),
                                                    VAR b))),
                                    SUB(VAR a, MUL(DIV(VAR a, VAR b),
                                                   VAR b))))))))
              end

        | g (PO.INLREM) =
              let val a = mkv() and b = mkv() and z = mkv()
               in FN(z, lt_ipair,
                    LET(a, SELECT(0,VAR z),
                      LET(b, SELECT(1,VAR z),
                          SUB(VAR a, MUL(DIV(VAR a,VAR b),VAR b)))))
              end

        | g (PO.INLMIN) =
              let val x = mkv() and y = mkv() and z = mkv()
               in FN(z, lt_ipair,
                    LET(x, SELECT(0,VAR z),
                       LET(y, SELECT(1,VAR z),
                         COND(APP(cmpOp(PO.ILT), RECORD[VAR x,VAR y]),
                              VAR x, VAR y))))
              end
        | g (PO.INLMAX) =
              let val x = mkv() and y = mkv() and z = mkv()
               in FN(z, lt_ipair,
                    LET(x, SELECT(0,VAR z),
                       LET(y, SELECT(1,VAR z),
                         COND(APP(cmpOp(PO.IGT), RECORD[VAR x,VAR y]),
                              VAR x, VAR y))))
              end
        | g (PO.INLABS) =
              let val x = mkv()
               in FN(x, lt_int,
                     COND(APP(cmpOp(PO.IGT), RECORD[VAR x,INT 0]),
                          VAR x, APP(inegOp(PO.INEG), VAR x)))
              end
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

        | g p = PRIM(p, lt, ts) 

   in g prim
  end (* function transPrim *)

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
fun mkVar (v as V.VALvar{access, info, typ, path}, d) = 
      mkAccInfo(access, info, fn () => toLty d (!typ), getNameOp path)
  | mkVar _ = bug "unexpected vars in mkVar"

fun mkVE (v as V.VALvar {info=II.INL_PRIM(p, typ), ...}, ts, d) = 
      (case (p, ts)
        of (PO.POLYEQL, [t]) => eqGen(typ, t, toTcLt d)
         | (PO.POLYNEQ, [t]) => composeNOT(eqGen(typ, t, toTcLt d), toLty d t)
         | (PO.INLMKARRAY, [t]) => 
                let val dict = 
                      {default = coreAcc "mkNormArray",
                       table = [([LT.tcc_real], coreAcc "mkRealArray")]}
                 in GENOP (dict, p, toLty d typ, map (toTyc d) ts)
                end
	 | (PO.RAW_CCALL NONE, [a, b, c]) =>
	   let val i = SOME { c_proto = CProto.decode b,
			      ml_flt_args = CProto.flt_args a,
			      ml_flt_res_opt = CProto.flt_res c }
		   handle CProto.BadEncoding => NONE
	    in PRIM (PO.RAW_CCALL i, toLty d typ, map (toTyc d) ts)
	   end
         | _ => transPrim(p, (toLty d typ), map (toTyc d) ts))
  | mkVE (v, [], d) = mkVar(v, d)
  | mkVE (v, ts, d) = TAPP(mkVar(v, d), map (toTyc d) ts)

fun mkCE (TP.DATACON{const, rep, name, typ, ...}, ts, apOp, d) = 
  let val lt = toDconLty d typ
      val rep' = mkRep(rep, lt, name)
      val dc = (name, rep', lt)
      val ts' = map (toTyc d) ts
   in if const then CON'(dc, ts', unitLexp)
      else (case apOp
             of SOME le => CON'(dc, ts', le)
              | NONE => 
                 let val (argT, _) = LT.ltd_parrow(LT.lt_pinst(lt, ts'))
                     val v = mkv()
                  in FN(v, argT, CON'(dc, ts', VAR v))
                 end)
  end 

fun mkStr (s as M.STR { access, info, ... }, d) =
    mkAccInfo(access, info, fn () => strLty(s, d, compInfo), NONE)
  | mkStr _ = bug "unexpected structures in mkStr"

fun mkFct (f as M.FCT { access, info, ... }, d) =
    mkAccInfo(access, info, fn () => fctLty(f, d, compInfo), NONE)
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
 *    val mkVBs  : Absyn.vb list * depth -> Lambda.lexp -> Lambda.lexp     *
 *    val mkRVBs : Absyn.rvb list * depth -> Lambda.lexp -> Lambda.lexp    *
 *    val mkEBs  : Absyn.eb list * depth -> Lambda.lexp -> Lambda.lexp     *
 *                                                                         *
 ***************************************************************************)
fun mkPE (exp, d, []) = mkExp(exp, d)
  | mkPE (exp, d, boundtvs) = 
      let val savedtvs = map ! boundtvs

          fun g (i, []) = ()
            | g (i, (tv as ref (TP.OPEN _))::rest) = 
                   (tv := TP.LBOUND{depth=d, num=i}; g(i+1,rest))
            | g (i, (tv as ref (TP.LBOUND _))::res) =
                   bug ("unexpected tyvar LBOUND in mkPE")
            | g _ = bug "unexpected tyvar INSTANTIATED in mkPE"

          val _ = g(0, boundtvs) (* assign the LBOUND tyvars *)
          val exp' = mkExp(exp, DI.next d)

          fun h ([], []) = ()
            | h (a::r, b::z) = (b := a; h(r, z))
            | h _ = bug "unexpected cases in mkPE"

          val _ = h(savedtvs, boundtvs)  (* recover *)
          val len = length(boundtvs)
       
       in TFN(LT.tkc_arg(len), exp')
      end

and mkVBs (vbs, d) =
  let fun eqTvs ([], []) = true
        | eqTvs (a::r, (TP.VARty b)::s) = if (a=b) then eqTvs(r, s) else false
        | eqTvs _ = false

      fun g (VB{pat=VARpat(V.VALvar{access=DA.LVAR v, ...}),
                exp as VARexp (ref (w as (V.VALvar _)), instys),
                boundtvs=tvs, ...}, b) = 
              if eqTvs(tvs, instys) then LET(v, mkVar(w, d), b)
              else LET(v, mkPE(exp, d, tvs), b)

        | g (VB{pat=VARpat(V.VALvar{access=DA.LVAR v, ...}),
                exp, boundtvs=tvs, ...}, b) = LET(v, mkPE(exp, d, tvs), b)

        | g (VB{pat=CONSTRAINTpat(VARpat(V.VALvar{access=DA.LVAR v, ...}),_),
                exp, boundtvs=tvs, ...}, b) = LET(v, mkPE(exp, d, tvs), b)

        | g (VB{pat, exp, boundtvs=tvs, ...}, b) =
              let val ee = mkPE(exp, d, tvs)
                  val rules = [(fillPat(pat, d), b), (WILDpat, unitLexp)]
                  val rootv = mkv()
                  fun finish x = LET(rootv, ee, x)
               in MC.bindCompile(env, rules, finish, rootv, toTcLt d, complain)
              end
   in fold g vbs
  end

and mkRVBs (rvbs, d) =
  let fun g (RVB{var=V.VALvar{access=DA.LVAR v, typ=ref ty, ...},
                 exp, boundtvs=tvs, ...}, (vlist, tlist, elist)) = 
               let val ee = mkExp(exp, d) (* was mkPE(exp, d, tvs) *)
                       (* we no longer track type bindings at RVB anymore ! *)
                   val vt = toLty d ty
                in (v::vlist, vt::tlist, ee::elist)
               end
        | g _ = bug "unexpected valrec bindings in mkRVBs"

      val (vlist, tlist, elist) = foldr g ([], [], []) rvbs

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
 *    val mkStrexp : Absyn.strexp * depth -> Lambda.lexp                   *
 *    val mkFctexp : Absyn.fctexp * depth -> Lambda.lexp                   *
 *    val mkStrbs  : Absyn.strb list * depth -> Lambda.lexp -> Lambda.lexp *
 *    val mkFctbs  : Absyn.fctb list * depth -> Lambda.lexp -> Lambda.lexp *
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
                   val nd = DI.next d
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
 *    val mkExp : A.exp * DI.depth -> L.lexp                               *
 *    val mkDec : A.dec * DI.depth -> L.lexp -> L.lexp                     *
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

      and g (VARexp (ref v, ts)) = mkVE(v, ts, d)

        | g (CONexp (dc, ts)) = mkCE(dc, ts, NONE, d)
        | g (APPexp (CONexp(dc, ts), e2)) = mkCE(dc, ts, SOME(g e2), d)

        | g (INTexp (s, t)) =
             ((if TU.equalType (t, BT.intTy) then INT (LN.int s)
               else if TU.equalType (t, BT.int32Ty) then INT32 (LN.int32 s)
                    else bug "translate INTexp")
               handle Overflow => (repErr "int constant too large"; INT 0))

        | g (WORDexp(s, t)) =
             ((if TU.equalType (t, BT.wordTy) then WORD (LN.word s)
               else if TU.equalType (t, BT.word8Ty) 
                    then WORD (LN.word8 s)
                    else if TU.equalType (t, BT.word32Ty) 
                         then WORD32 (LN.word32 s) 
                         else (ppType t;
			       bug "translate WORDexp"))
               handle Overflow => (repErr "word constant too large"; INT 0))

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
(*
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
        | g (HANDLEexp (e, HANDLER(FNexp(l, ty)))) =
             let val rootv = mkv()
                 fun f x = FN(rootv, tLty ty, x)
                 val l' = mkRules l
              in HANDLE(g e, MC.handCompile(env, l', f, 
                                            rootv, toTcLt d, complain))
             end

        | g (FNexp (l, ty)) = 
             let val rootv = mkv()
                 fun f x = FN(rootv, tLty ty, x)
              in MC.matchCompile (env, mkRules l, f, rootv, toTcLt d, complain)
             end

        | g (CASEexp (ee, l, isMatch)) = 
             let val rootv = mkv()
                 val ee' = g ee
                 fun f x = LET(rootv, ee', x)
                 val l' = mkRules l
              in if isMatch 
                 then MC.matchCompile (env, l', f, rootv, toTcLt d, complain)
                 else MC.bindCompile (env, l', f, rootv, toTcLt d, complain)
             end

        | g (LETexp (dc, e)) = mkDec (dc, d) (g e)

        | g e = 
             EM.impossibleWithBody "untranslateable expression"
              (fn ppstrm => (PP.add_string ppstrm " expression: ";
                            PPAbsyn.ppExp (env,NONE) ppstrm (e, !ppDepth)))

   in g exp
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

(** translating the ML absyn into the PLambda expression *)
val body = mkDec (rootdec, DI.top) exportLexp

(** wrapping up the body with the imported variables *)
val (plexp, imports) = wrapPidInfo (body, Map.listItemsi (!persmap))

fun prGen (flag,printE) s e =
  if !flag then (say ("\n\n[After " ^ s ^ " ...]\n\n"); printE e) else ()
val _ = prGen(Control.FLINT.print, PPLexp.printLexp) "Translate" plexp

(** normalizing the plambda expression into FLINT *)
val flint = FlintNM.norm plexp

in {flint = flint, imports = imports}
end (* function transDec *)

end (* top-level local *)
end (* structure Translate *)


