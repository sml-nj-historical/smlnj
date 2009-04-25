(* Copyright 2006 by the Standard ML Fellowship *)
(* primopmap.sml *)

(* The module PrimOpTypeMap provides a table mapping all the primops formerly
 * defined in the InLine structure to their intrinsic types. The table is
 * indexed by the primop names used in InLine.
 * The table is given in the form of the primopTypeMap function, which maps
 * a primop name to the "intrinsic" type (the type formerly used for
 * the primop in InLine).
 *
 * The translate phase will use this table to lookup names in the primId field
 * of variables when translating variables.
 * 
 * PrimOpTypeMap is used in Semant/statenv/prim.sml and FLINT/trans/translate.sml.
 *)

signature PRIMOP_TYPE_MAP =
sig 
  val primopTypeMap : string -> Types.ty option
end (* signature PRIMOP_MAP *)


structure PrimOpTypeMap : PRIMOP_TYPE_MAP = 
struct

structure T = Types
structure BT = BasicTypes

structure StringKey : ORD_KEY =
struct
  type ord_key = string
  val compare = String.compare
end

structure RBMap = RedBlackMapFn(StringKey)

fun bug msg = ErrorMsg.impossible("PrimOpMap: " ^ msg)

(**************************************************************************
 *                 BUILDING A COMPLETE LIST OF PRIMOPS                    *
 **************************************************************************)

(* 
fun bits size oper = P.ARITH{oper=oper, overflow=false, kind=P.INT size}
val bits31 = bits 31		
val bits32 = bits 32		

fun int size oper = P.ARITH{oper=oper, overflow=true, kind=P.INT size}
val int31 = int 31
val int32 = int 32

fun word size oper = P.ARITH{oper=oper, overflow=false, kind=P.UINT size}
val word32 = word 32
val word31 = word 31
val word8  = word 8

fun purefloat size oper = P.ARITH{oper=oper,overflow=false,kind=P.FLOAT size}
val purefloat64 = purefloat 64	

fun cmp kind oper = P.CMP{oper=oper, kind=kind}
val int31cmp = cmp (P.INT 31)
val int32cmp = cmp (P.INT 32)

val word32cmp = cmp (P.UINT 32)
val word31cmp = cmp (P.UINT 31)
val word8cmp  = cmp (P.UINT 8)

val float64cmp = cmp (P.FLOAT 64)
 *)
val v1 = T.IBOUND 0
val v2 = T.IBOUND 1
val v3 = T.IBOUND 2

val tu = BT.tupleTy
fun ar(t1,t2) = BT.--> (t1, t2)

fun ap(tc,l) = T.CONty(tc, l)
fun cnt t = T.CONty(BT.contTycon,[t])
fun ccnt t = T.CONty(BT.ccontTycon,[t])
fun rf t = T.CONty(BT.refTycon,[t])
fun ay t = T.CONty(BT.arrayTycon,[t])
fun vct t = T.CONty(BT.vectorTycon,[t])

val u = BT.unitTy
val bo = BT.boolTy
val i = BT.intTy
val i32 = BT.int32Ty
val i64 = BT.int64Ty
val inf = BT.intinfTy
val w8 = BT.word8Ty
val w = BT.wordTy
val w32 = BT.word32Ty
val w64 = BT.word64Ty
val f64 = BT.realTy
val s  = BT.stringTy

fun p0 t = t
fun p1 t = T.POLYty {sign=[false], tyfun=T.TYFUN {arity=1, body=t}}
fun ep1 t = T.POLYty {sign=[true], tyfun=T.TYFUN {arity=1, body=t}}
fun p2 t = T.POLYty {sign=[false,false], tyfun=T.TYFUN {arity=2, body=t}}
fun p3 t = T.POLYty {sign=[false,false,false], tyfun=T.TYFUN {arity=3, body=t}}
(*
fun sub kind = P.NUMSUBSCRIPT{kind=kind, checked=false, immutable=false}
fun chkSub kind = P.NUMSUBSCRIPT{kind=kind, checked=true, immutable=false}

fun subv kind = P.NUMSUBSCRIPT{kind=kind, checked=false, immutable=true}
fun chkSubv kind = P.NUMSUBSCRIPT{kind=kind, checked=true, immutable=true}

fun update kind = P.NUMUPDATE {kind=kind, checked=false}
fun chkUpdate kind = P.NUMUPDATE {kind=kind, checked=true}
*)
val numSubTy = p2(ar(tu[v1,i],v2))
val numUpdTy = p2(ar(tu[v1,i,v2],u))

fun unf t = p0(ar(t,t))
fun binf t = p0(ar(tu[t,t],t))
fun binp t = p0(ar(tu[t,t],bo))
fun shifter t = p0(ar(tu[t,w],t))

val w32_i32 = p0(ar(w32,i32))
val w32_f64 = p0(ar(w32,f64))
val w32w32_u = p0(ar(tu[w32,w32],u))
val w32i32_u = p0(ar(tu[w32,i32],u))
val w32f64_u = p0(ar(tu[w32,f64],u))

val i_x      = p1(ar(i,v1))
val xw32_w32 = p1(ar(tu[v1,w32],w32))
val xw32_i32 = p1(ar(tu[v1,w32],i32))
val xw32_f64 = p1(ar(tu[v1,w32],f64))
val xw32w32_u = p1(ar(tu[v1,w32,w32],u))
val xw32i32_u = p1(ar(tu[v1,w32,i32],u))
val xw32f64_u = p1(ar(tu[v1,w32,f64],u))

val b_b = unf bo

val f64_i = p0(ar(f64,i))
val i_f64 = p0(ar(i,f64))
val i32_f64 = p0(ar(i32,f64))

val w32_i = p0(ar(w32,i))
val i32_i = p0(ar(i32,i))

val i_i32 = p0(ar(i,i32))
val i_w32 = p0(ar(i,w32))

val w32_w = p0(ar(w32,w))
val i32_w = p0(ar(i32,w))

val w_w32 = p0(ar(w,w32))
val w_i32 = p0(ar(w,i32))

val w_i = p0(ar(w,i))
val i_w = p0(ar(i,w))

val w32_i32 = p0(ar(w32,i32))
val i32_w32 = p0(ar(i32,w32))

val i_i = unf i
val ii_i = binf i
val ii_b = binp i
val iw_i = shifter i

val w_w = unf w
val ww_w = binf w
val ww_b = binp w

val i32_i32 = unf i32
val i32i32_i32 = binf i32
val i32i32_b = binp i32

val w32_w32 = unf w32
val w32w32_w32 = binf w32
val w32w32_b = binp w32
val w32w_w32 = shifter w32

val w8_w8 = unf w8
val w8w8_w8 = binf w8
val w8w8_b = binp w8
val w8w_w8 = shifter w8

val f64_b = p0(ar(f64,bo))
val f64_f64 = unf f64
val f64f64_f64 = binf f64
val f64f64_b = binp f64

val w8_i = p0(ar(w8,i))
val w8_i32 = p0(ar(w8,i32))
val w8_w32 = p0(ar(w8,w32))
val i_w8 = p0(ar(i,w8))
val i32_w8 = p0(ar(i32,w8))
val w32_w8 = p0(ar(w32,w8))

val inf_i   = p0(ar(inf,i))
val inf_i32 = p0(ar(inf,i32))
val inf_i64 = p0(ar(inf,i64))
val inf_w8  = p0(ar(inf,w8))
val inf_w   = p0(ar(inf,w))
val inf_w32 = p0(ar(inf,w32))
val inf_w64 = p0(ar(inf,w64))
val i_inf   = p0(ar(i,inf))
val i32_inf = p0(ar(i32,inf))
val i64_inf = p0(ar(i64,inf))
val w8_inf  = p0(ar(w8,inf))
val w_inf   = p0(ar(w,inf))
val w32_inf = p0(ar(w32,inf))
val w64_inf = p0(ar(w64,inf))

val w64_pw32 = p0(ar(w64,tu[w32,w32]))
val pw32_w64 = p0(ar(tu[w32,w32],w64))
val i64_pw32 = p0(ar(i64,tu[w32,w32]))
val pw32_i64 = p0(ar(tu[w32,w32],i64))

val cc_b = binp BT.charTy

(* The type of the RAW_CCALL primop (as far as the type checker is concerned)
 * is:
 *    word32 * 'a * 'b -> 'd
 * However, the primop cannot be used without having 'a, 'b, and 'c
 * monomorphically instantiated.  In particular, 'a will be the type of the
 * ML argument list, 'c will be the type of the result, and 'b
 * will be a type of a fake arguments.  The idea is that 'b will be
 * instantiated with some ML type that encodes the type of the actual
 * C function in order to be able to generate code according to the C
 * calling convention.
 * (In other words, 'b will be a completely ad-hoc encoding of a CTypes.c_proto
 * value in ML types.  The encoding also contains information about
 * calling conventions and reentrancy.)
 *)
val rccType = p3(ar(tu[w32,v1,v2],v3))

(*
 * I made an effort to eliminate the cases where type info for primops
 * is left NONE because this is, in fact, incorrect.  (As long as they
 * are left at NONE, there are correct ML programs that trigger internal
 * compiler errors.)
 *    - M.Blume (1/2001)
 *)


val empty = RBMap.empty

(* Below there is a bunch of very long list literals which would create
 * huge register pressure on the compiler.  We construct them backwards
 * using an alternative "cons" that takes its two arguments in opposite
 * order.  This effectively puts the lists' ends to the left and alleviates
 * this effect. (Stupid ML trick No. 21b) (Blume, 1/2001) *)
infix :-:
fun m :-: (name,entry) = RBMap.insert(m,name,entry)

val primopTypes =
    empty :-:
       ("callcc",	 (p1(ar(ar(cnt(v1),v1),v1)))) :-:
       ("throw",	 (p2(ar(cnt(v1),ar(v1,v2))))) :-:
       ("capture",	 (p1(ar(ar(ccnt(v1),v1),v1)))) :-:
       ("isolate",	 (p1(ar(ar(v1,u),cnt(v1))))) :-:
       ("cthrow",	 (p2(ar(ccnt(v1),ar(v1,v2))))) :-:
       ("!",		 (p1(ar(rf(v1),v1)))) :-:
       (":=",	         (p1(ar(tu[rf(v1),v1],u)))) :-:
       ("makeref",	 (p1(ar(v1,rf(v1))))) :-:
       ("boxed",	 (p1(ar(v1,bo)))) :-:
       ("unboxed",	 (p1(ar(v1,bo)))) :-:
       ("cast",	         (p2(ar(v1,v2)))) :-:
       ("=",		 (ep1(ar(tu[v1,v1],bo)))) :-:
       ("<>",	         (ep1(ar(tu[v1,v1],bo)))) :-:
       ("ptreql",	 (p1(ar(tu[v1,v1],bo)))) :-:
       ("ptrneq",	 (p1(ar(tu[v1,v1],bo)))) :-:
       ("getvar",	 (p1(ar(u,v1)))) :-:
       ("setvar",	 (p1(ar(v1,u)))) :-:
       ("setpseudo",	 (p1(ar(tu[v1,i],u)))) :-:
       ("getpseudo",	 (p1(ar(i,v1)))) :-:
       ("mkspecial",     (p2(ar(tu[i,v1],v2)))) :-:
       ("getspecial",    (p1(ar(v1,i)))) :-:
       ("setspecial",    (p1(ar(tu[v1,i],u)))) :-:
       ("gethdlr",	 (p1(ar(u,cnt(v1))))) :-:
       ("sethdlr",	 (p1(ar(cnt(v1),u)))) :-:
       ("gettag", 	 (p1(ar(v1,i)))) :-:
       ("setmark",	 (p1(ar(v1,u)))) :-:
       ("dispose",	 (p1(ar(v1,u)))) :-:
       ("compose",	 (p3(ar(tu[ar(v2,v3),ar(v1,v2)],ar(v1,v3))))) :-:
       ("before",	 (p2(ar(tu[v1,v2],v1)))) :-:
       ("ignore",        (p1(ar(v1,u)))) :-:
       ("identity",      (p1(ar(v1,v1)))) :-:
			 
       			 
       ("length",	 (p1(ar(v1,i)))) :-:
       ("objlength",	 (p1(ar(v1,i)))) :-:

       (*  
        * I believe the following five primops should not be exported into
        * the InLine structure. (ZHONG) 
        *)
       (* So we take them out... (Matthias)
       ("boxedupdate",   P.BOXEDUPDATE,   ?) :-:
       ("getrunvec",	 P.GETRUNVEC,     ?) :-:
       ("uselvar",	 P.USELVAR,       ?) :-:
       ("deflvar",	 P.DEFLVAR,       ?) :-:
       *)

       (* I put this one back in so tprof can find it in _Core
	* instead of having to construct it ... (Matthias) *)
       ("unboxedupdate", (p1(ar(tu[ay(v1),i,v1],u)))) :-:
       			 
       ("inlnot",	 (b_b)) :-:
       ("floor",         (f64_i)) :-:
       ("round",         (f64_i)) :-:
       ("real",          (i_f64)) :-:
       ("real32",        (i32_f64)) :-:
       			 
       ("ordof",         (numSubTy)) :-:
       ("store",         (numUpdTy)) :-:
       ("inlbyteof",     (numSubTy)) :-:
       ("inlstore",      (numUpdTy)) :-:
       ("inlordof",      (numSubTy)) :-:

       (*** polymorphic array and vector ***)
       ("mkarray",       (p1(ar(tu[i,v1],ay(v1))))) :-:
       ("arrSub", 	 (p1(ar(tu[ay(v1),i],v1)))) :-:
       ("arrChkSub",	 (p1(ar(tu[ay(v1),i],v1)))) :-:
       ("vecSub",	 (p1(ar(tu[vct(v1),i],v1)))) :-:
       ("vecChkSub",	 (p1(ar(tu[vct(v1),i],v1)))) :-:
       ("arrUpdate",	 (p1(ar(tu[ay(v1),i,v1],u)))) :-:
       ("arrChkUpdate",  (p1(ar(tu[ay(v1),i,v1],u)))) :-:

       (* new array representations *)
	("newArray0",	 (p1(ar(u,v1)))) :-:
	("getSeqData",	 (p2(ar(v1, v2)))) :-:
	("recordSub",	 (p2(ar(tu[v1,i],v2)))) :-:
	("raw64Sub",	 (p1(ar(tu[v1,i],f64)))) :-:

       (* *** conversion primops ***
	*   There are certain duplicates for the same primop (but with
	*   different types).  In such a case, the "canonical" name
	*   of the primop has been extended using a simple suffix
	*   scheme. *)
       ("test_32_31_w",  (w32_i)) :-:
       ("test_32_31_i",  (i32_i)) :-:

       ("testu_31_31",   (w_i)) :-:

       ("testu_32_31",   (w32_i)) :-:

       ("testu_32_32",   (w32_i32)) :-:

       ("copy_32_32_ii", (i32_i32)) :-:
       ("copy_32_32_wi", (w32_i32)) :-:
       ("copy_32_32_iw", (i32_w32)) :-:
       ("copy_32_32_ww", (w32_w32)) :-:

       ("copy_31_31_ii", (i_i)) :-:
       ("copy_31_31_wi", (w_i)) :-:
       ("copy_31_31_iw", (i_w)) :-:

       ("copy_31_32_i",  (w_i32)) :-:
       ("copy_31_32_w",  (w_w32)) :-:

       ("copy_8_32_i",   (w8_i32)) :-:
       ("copy_8_32_w",   (w8_w32)) :-:

       ("copy_8_31",     (w8_i)) :-:

       ("extend_31_32_ii", (i_i32)) :-:
       ("extend_31_32_iw", (i_w32)) :-:
       ("extend_31_32_wi", (w_i32)) :-:
       ("extend_31_32_ww", (w_w32)) :-:

       ("extend_8_31",   (w8_i)) :-:

       ("extend_8_32_i", (w8_i32)) :-:
       ("extend_8_32_w", (w8_w32)) :-:

       ("trunc_32_31_i", (i32_w)) :-:
       ("trunc_32_31_w", (w32_w)) :-:

       ("trunc_31_8",    (i_w8)) :-:

       ("trunc_32_8_i",  (i32_w8)) :-:
       ("trunc_32_8_w",  (w32_w8)) :-:

       (* conversion primops involving intinf *)
       ("test_inf_31",   (inf_i))   :-:
       ("test_inf_32",   (inf_i32)) :-:
       ("test_inf_64",   (inf_i64)) :-:
       ("copy_8_inf",    (w8_inf))  :-:
       ("copy_8_inf_w",  (w8_inf))  :-:
       ("copy_31_inf_w", (w_inf))   :-:
       ("copy_32_inf_w", (w32_inf)) :-:
       ("copy_64_inf_w", (w64_inf)) :-:
       ("copy_31_inf_i", (i_inf))   :-:
       ("copy_32_inf_i", (i32_inf)) :-:
       ("copy_64_inf_i", (i64_inf)) :-:
       ("extend_8_inf",  (w8_inf))  :-:
       ("extend_8_inf_w",  (w8_inf))  :-:
       ("extend_31_inf_w", (w_inf)) :-:
       ("extend_32_inf_w", (w32_inf)) :-:
       ("extend_64_inf_w", (w64_inf)) :-:
       ("extend_31_inf_i", (i_inf)) :-:
       ("extend_32_inf_i", (i32_inf)) :-:
       ("extend_64_inf_i", (i64_inf)) :-:
       ("trunc_inf_8",   (inf_w8))  :-:
       ("trunc_inf_31",  (inf_w)) :-:
       ("trunc_inf_32",  (inf_w32)) :-:
       ("trunc_inf_64",  (inf_w64)) :-:
       
       (* primops to go between abstract and concrete representation of
	* 64-bit ints and words *)
       ("w64p",          (w64_pw32)) :-:
       ("p64w",          (pw32_w64)) :-:
       ("i64p",          (i64_pw32)) :-:
       ("p64i",          (pw32_i64)) :-:

       (* *** integer 31 primops ***
        *   Many of the i31 primops are being abused for different types
	*   (mostly Word8.word and also for char).  In these cases
	*   there are suffixed alternative versions of the primop
	*   (i.e., same primop, different type). *)
       ("i31add", 	 (ii_i)) :-:
       ("i31add_8", 	 (w8w8_w8)) :-:

       ("i31sub",	 (ii_i)) :-:
       ("i31sub_8",	 (w8w8_w8)) :-:

       ("i31mul",	 (ii_i)) :-:
       ("i31mul_8",	 (w8w8_w8)) :-:

       ("i31div",	 (ii_i)) :-:
       ("i31div_8",	 (w8w8_w8)) :-:

       ("i31mod",        (ii_i)) :-:
       ("i31mod_8",      (w8w8_w8)) :-:

       ("i31quot",	 (ii_i)) :-:

       ("i31rem",	 (ii_i)) :-:

       ("i31orb",	 (ii_i)) :-:
       ("i31orb_8",	 (w8w8_w8)) :-:

       ("i31andb",	 (ii_i)) :-:
       ("i31andb_8",	 (w8w8_w8)) :-:

       ("i31xorb",	 (ii_i)) :-:
       ("i31xorb_8",	 (w8w8_w8)) :-:

       ("i31notb",	 (i_i)) :-:
       ("i31notb_8",	 (w8_w8)) :-:

       ("i31neg",	 (i_i)) :-:
       ("i31neg_8",	 (w8_w8)) :-:

       ("i31lshift",	 (ii_i)) :-:
       ("i31lshift_8",	 (w8w_w8)) :-:

       ("i31rshift",	 (ii_i)) :-:
       ("i31rshift_8",	 (w8w_w8)) :-:

       ("i31lt",	 (ii_b)) :-:
       ("i31lt_8",	 (w8w8_b)) :-:
       ("i31lt_c",	 (cc_b)) :-:

       ("i31le",	 (ii_b)) :-:
       ("i31le_8",	 (w8w8_b)) :-:
       ("i31le_c",	 (cc_b)) :-:

       ("i31gt",	 (ii_b)) :-:
       ("i31gt_8",	 (w8w8_b)) :-:
       ("i31gt_c",	 (cc_b)) :-:

       ("i31ge", 	 (ii_b)) :-:
       ("i31ge_8", 	 (w8w8_b)) :-:
       ("i31ge_c", 	 (cc_b)) :-:

       ("i31ltu",	 (ii_b)) :-:
       ("i31geu",	 (ii_b)) :-:
       ("i31eq",	 (ii_b)) :-:
       ("i31ne",	 (ii_b)) :-:

       ("i31min",	 (ii_i)) :-:
       ("i31min_8",	 (w8w8_w8)) :-:
       ("i31max",	 (ii_i)) :-:
       ("i31max_8",	 (w8w8_w8)) :-:

       ("i31abs",	 (i_i)) :-:

       (*** integer 32 primops ***)
       ("i32mul",        (i32i32_i32)) :-:
       ("i32div",        (i32i32_i32)) :-:
       ("i32mod",        (i32i32_i32)) :-:
       ("i32quot",       (i32i32_i32)) :-:
       ("i32rem",        (i32i32_i32)) :-:
       ("i32add",        (i32i32_i32)) :-:
       ("i32sub",        (i32i32_i32)) :-:
       ("i32orb",        (i32i32_i32)) :-:
       ("i32andb",       (i32i32_i32)) :-:
       ("i32xorb",       (i32i32_i32)) :-:
       ("i32lshift",     (i32i32_i32)) :-:
       ("i32rshift",     (i32i32_i32)) :-:
       ("i32neg",        (i32_i32)) :-:
       ("i32lt",         (i32i32_b)) :-:
       ("i32le",         (i32i32_b)) :-:
       ("i32gt",         (i32i32_b)) :-:
       ("i32ge",         (i32i32_b)) :-:
       ("i32eq",         (i32i32_b)) :-:
       ("i32ne",         (i32i32_b)) :-:

       ("i32min",	 (i32i32_i32)) :-:
       ("i32max",	 (i32i32_i32)) :-:
       ("i32abs",	 (i32_i32)) :-:

       (*** float 64 primops ***)
       ("f64add", 	 (f64f64_f64)) :-:
       ("f64sub",	 (f64f64_f64)) :-:
       ("f64div", 	 (f64f64_f64)) :-:
       ("f64mul",	 (f64f64_f64)) :-:
       ("f64neg",	 (f64_f64)) :-:
       ("f64ge",	 (f64f64_b)) :-:
       ("f64gt",	 (f64f64_b)) :-:
       ("f64le",	 (f64f64_b)) :-:
       ("f64lt",	 (f64f64_b)) :-:
       ("f64eq",	 (f64f64_b)) :-:
       ("f64ne",	 (f64f64_b)) :-:
       ("f64sgn",	 (f64_b)) :-:
       ("f64abs",	 (f64_f64)) :-:

       ("f64sin",	 (f64_f64)) :-:
       ("f64cos",	 (f64_f64)) :-:
       ("f64tan",	 (f64_f64)) :-:
       ("f64sqrt",	 (f64_f64)) :-:

       ("f64min",	 (f64f64_f64)) :-:
       ("f64max",	 (f64f64_f64)) :-:

       (*** float64 array ***)	
       ("f64Sub",	 (numSubTy)) :-:
       ("f64chkSub",	 (numSubTy)) :-:
       ("f64Update",	 (numUpdTy)) :-:
       ("f64chkUpdate",  (numUpdTy)) :-:

       (*** word8 primops ***)
       (* 
        * In the long run, we plan to represent WRAPPED word8 tagged, and 
        * UNWRAPPED untagged. But right now, we represent both of them 
        * tagged, with 23 high-order zero bits and 1 low-order 1 bit.
        * In this representation, we can use the comparison and (some of 
        * the) bitwise operators of word31; but we cannot use the shift 
        * and arithmetic operators.
        *
        * WARNING: THIS IS A TEMPORARY HACKJOB until all the word8 primops 
        * are correctly implemented.
        *
        * ("w8mul",	word8 (P.* ),      	w8w8_w8)) :-:
	* ("w8div",	word8 (P./),      	w8w8_w8)) :-:
	* ("w8add",	word8 (P.+),      	w8w8_w8)) :-:
	* ("w8sub",	word8 (P.-),      	w8w8_w8)) :-:
        *		
        * ("w8notb",	word31 P.NOTB,      	w8_w8)) :-:
	* ("w8rshift",	word8 P.RSHIFT,      	w8w_w8)) :-:
	* ("w8rshiftl",	word8 P.RSHIFTL,      	w8w_w8)) :-:
	* ("w8lshift",	word8 P.LSHIFT,      	w8w_w8)) :-:
        *
	* ("w8toint",   P.ROUND{floor=true, 
        *                     fromkind=P.UINT 8, 
        *                     tokind=P.INT 31},   w8_i)) :-:
	* ("w8fromint", P.REAL{fromkind=P.INT 31,
        *                    tokind=P.UINT 8},    i_w8)) :-:
        *)
  
       ("w8orb",	(w8w8_w8)) :-:
       ("w8xorb",	(w8w8_w8)) :-:
       ("w8andb",	(w8w8_w8)) :-:
       	 		
       ("w8gt",	        (w8w8_b)) :-:
       ("w8ge",	        (w8w8_b)) :-:
       ("w8lt",	        (w8w8_b)) :-:
       ("w8le",		(w8w8_b)) :-:
       ("w8eq",		(w8w8_b)) :-:
       ("w8ne",		(w8w8_b)) :-:

       (*** word8 array and vector ***)
       ("w8Sub",	(numSubTy)) :-:
       ("w8chkSub",	(numSubTy)) :-:
       ("w8subv",	(numSubTy)) :-:
       ("w8chkSubv",	(numSubTy)) :-:
       ("w8update",	(numUpdTy)) :-:
       ("w8chkUpdate",  (numUpdTy)) :-:

       (* word31 primops *)
       ("w31mul",	(ww_w)) :-:
       ("w31div",	(ww_w)) :-:
       ("w31mod",	(ww_w)) :-:
       ("w31add",	(ww_w)) :-:
       ("w31sub",	(ww_w)) :-:
       ("w31orb",	(ww_w)) :-:
       ("w31xorb",	(ww_w)) :-:
       ("w31andb",	(ww_w)) :-:
       ("w31notb",	(w_w)) :-:
       ("w31neg",       (w_w)) :-:
       ("w31rshift",	(ww_w)) :-:
       ("w31rshiftl",   (ww_w)) :-:
       ("w31lshift",	(ww_w)) :-:
       ("w31gt",	(ww_b)) :-:
       ("w31ge",	(ww_b)) :-:
       ("w31lt",	(ww_b)) :-:
       ("w31le",	(ww_b)) :-:
       ("w31eq",	(ww_b)) :-:
       ("w31ne",	(ww_b)) :-:
       ("w31ChkRshift", (ww_w)) :-:
       ("w31ChkRshiftl",(ww_w)) :-:
       ("w31ChkLshift", (ww_w)) :-:

       ("w31min",	(ww_w)) :-:
       ("w31max",	(ww_w)) :-:
       
       (* (pseudo-)word8 primops *)
       ("w31mul_8",	(w8w8_w8)) :-:
       ("w31div_8",	(w8w8_w8)) :-:
       ("w31mod_8",	(w8w8_w8)) :-:
       ("w31add_8",	(w8w8_w8)) :-:
       ("w31sub_8",	(w8w8_w8)) :-:
       ("w31orb_8",	(w8w8_w8)) :-:
       ("w31xorb_8",	(w8w8_w8)) :-:
       ("w31andb_8",	(w8w8_w8)) :-:
       ("w31notb_8",	(w8_w8)) :-:
       ("w31neg_8",     (w8_w8)) :-:
       ("w31rshift_8",	(w8w_w8)) :-:
       ("w31rshiftl_8", (w8w_w8)) :-:
       ("w31lshift_8",	(w8w_w8)) :-:
       ("w31gt_8",	(w8w8_b)) :-:
       ("w31ge_8",	(w8w8_b)) :-:
       ("w31lt_8",	(w8w8_b)) :-:
       ("w31le_8",	(w8w8_b)) :-:
       ("w31eq_8",	(w8w8_b)) :-:
       ("w31ne_8",	(w8w8_b)) :-:
       ("w31ChkRshift_8", (w8w_w8)) :-:
       ("w31ChkRshiftl_8",(w8w_w8)) :-:
       ("w31ChkLshift_8", (w8w_w8)) :-:

       ("w31min_8",	(w8w8_w8)) :-:
       ("w31max_8",	(w8w8_w8)) :-:

       (*** word32 primops ***)
       ("w32mul",	(w32w32_w32)) :-:
       ("w32div",	(w32w32_w32)) :-:
       ("w32mod",	(w32w32_w32)) :-:
       ("w32add",	(w32w32_w32)) :-:
       ("w32sub",	(w32w32_w32)) :-:
       ("w32orb",	(w32w32_w32)) :-:
       ("w32xorb",	(w32w32_w32)) :-:
       ("w32andb",	(w32w32_w32)) :-:
       ("w32notb",	(w32_w32)) :-:
       ("w32neg",	(w32_w32)) :-:
       ("w32rshift",	(w32w_w32)) :-:
       ("w32rshiftl",   (w32w_w32)) :-:
       ("w32lshift",	(w32w_w32)) :-:
       ("w32gt",	(w32w32_b)) :-:
       ("w32ge",	(w32w32_b)) :-:
       ("w32lt",	(w32w32_b)) :-:
       ("w32le",	(w32w32_b)) :-:
       ("w32eq",	(w32w32_b)) :-:
       ("w32ne",	(w32w32_b)) :-:
       ("w32ChkRshift", (w32w_w32)) :-:
       ("w32ChkRshiftl",(w32w_w32)) :-:
       ("w32ChkLshift", (w32w_w32)) :-:

       ("w32min",	(w32w32_w32)) :-:
       ("w32max",	(w32w32_w32)) :-:

       (* experimental C FFI primops *)
       ("raww8l",       (w32_w32)) :-:
       ("rawi8l",       (w32_i32)) :-:
       ("raww16l",      (w32_w32)) :-:
       ("rawi16l",      (w32_i32)) :-:
       ("raww32l",      (w32_w32)) :-:
       ("rawi32l",      (w32_i32)) :-:
       ("rawf32l",      (w32_f64)) :-:
       ("rawf64l",      (w32_f64)) :-:
       ("raww8s",       (w32w32_u)) :-:
       ("rawi8s",       (w32i32_u)) :-:
       ("raww16s",      (w32w32_u)) :-:
       ("rawi16s",      (w32i32_u)) :-:
       ("raww32s",      (w32w32_u)) :-:
       ("rawi32s",      (w32i32_u)) :-:
       ("rawf32s",      (w32f64_u)) :-:
       ("rawf64s",      (w32f64_u)) :-:
       ("rawccall",     (rccType)) :-:

          (* Support for direct construction of C objects on ML heap.
           * rawrecord builds a record holding C objects on the heap.
           * rawselectxxx index on this record.  They are of type:
           *    'a * Word32.word -> Word32.word
           * The 'a is to guarantee that the compiler will treat
           * the record as a ML object, in case it passes thru a gc boundary.
           * rawupdatexxx writes to the record.
           *) 

       ("rawrecord",    (i_x)) :-:
       ("rawrecord64",  (i_x)) :-:

       ("rawselectw8",  (xw32_w32)) :-:
       ("rawselecti8",  (xw32_i32)) :-:
       ("rawselectw16", (xw32_w32)) :-:
       ("rawselecti16", (xw32_i32)) :-:
       ("rawselectw32", (xw32_w32)) :-:
       ("rawselecti32", (xw32_i32)) :-:
       ("rawselectf32", (xw32_f64)) :-:
       ("rawselectf64", (xw32_f64)) :-:

       ("rawupdatew8",  (xw32w32_u)) :-:
       ("rawupdatei8",  (xw32i32_u)) :-:
       ("rawupdatew16", (xw32w32_u)) :-:
       ("rawupdatei16", (xw32i32_u)) :-:
       ("rawupdatew32", (xw32w32_u)) :-:
       ("rawupdatei32", (xw32i32_u)) :-:
       ("rawupdatef32", (xw32f64_u)) :-:
       ("rawupdatef64", (xw32f64_u))

fun primopTypeMap name = RBMap.find(primopTypes,name)

end (* structure PrimOpTypeMap *)
