(* Copyright 2006 by the Standard ML Fellowship *)
(* primopmap.sml *)

(* The module PrimOpMap provides a table of all the primops formerly defined in
 * the InLine structure, indexed by the primop names defined used in InLine.
 * The table is given in the form of the primopMap function, which maps a primop
 * name to the primop value and its "intrinsic" type (the type formerly used for
 * the primop in InLine).
 *
 * The translate phase will use this table to lookup names in the primId field
 * of variables when translating variables.
 *)

signature PRIMOP_MAP =
sig 
  val primopMap : string -> PrimOp.primop option
end (* signature PRIMOP_MAP *)


structure PrimOpMap : PRIMOP_MAP = 
struct

  structure T = Types
  structure BT = BasicTypes
  structure P = PrimOp

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

fun sub kind = P.NUMSUBSCRIPT{kind=kind, checked=false, immutable=false}
fun chkSub kind = P.NUMSUBSCRIPT{kind=kind, checked=true, immutable=false}

fun subv kind = P.NUMSUBSCRIPT{kind=kind, checked=false, immutable=true}
fun chkSubv kind = P.NUMSUBSCRIPT{kind=kind, checked=true, immutable=true}

fun update kind = P.NUMUPDATE {kind=kind, checked=false}
fun chkUpdate kind = P.NUMUPDATE {kind=kind, checked=true}

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

val primops =
    empty :-:
       ("callcc",	 (P.CALLCC)) :-:
       ("throw",	 (P.THROW)) :-:
       ("capture",	 (P.CAPTURE)) :-:
       ("isolate",	 (P.ISOLATE)) :-:
       ("cthrow",	 (P.THROW)) :-:
       ("!",		 (P.DEREF)) :-:
       (":=",	         (P.ASSIGN)) :-:
       ("makeref",	 (P.MAKEREF)) :-:
       ("boxed",	 (P.BOXED)) :-:
       ("unboxed",	 (P.UNBOXED)) :-:
       ("cast",	         (P.CAST)) :-:
       ("=",		 (P.POLYEQL)) :-:
       ("<>",	         (P.POLYNEQ)) :-:
       ("ptreql",	 (P.PTREQL)) :-:
       ("ptrneq",	 (P.PTRNEQ)) :-:
       ("getvar",	 (P.GETVAR)) :-:
       ("setvar",	 (P.SETVAR)) :-:
       ("setpseudo",	 (P.SETPSEUDO)) :-:
       ("getpseudo",	 (P.GETPSEUDO)) :-:
       ("mkspecial",     (P.MKSPECIAL)) :-:
       ("getspecial",    (P.GETSPECIAL)) :-:
       ("setspecial",    (P.SETSPECIAL)) :-:
       ("gethdlr",	 (P.GETHDLR)) :-:
       ("sethdlr",	 (P.SETHDLR)) :-:
       ("gettag", 	 (P.GETTAG)) :-:
       ("setmark",	 (P.SETMARK)) :-:
       ("dispose",	 (P.DISPOSE)) :-:
       ("compose",	 (P.INLCOMPOSE)) :-:
       ("before",	 (P.INLBEFORE)) :-:
       ("ignore",        (P.INLIGNORE)) :-:
       ("identity",      (P.INLIDENTITY)) :-:
			 
       			 
       ("length",	 (P.LENGTH)) :-:
       ("objlength",	 (P.OBJLENGTH)) :-:

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
       ("unboxedupdate", (P.UNBOXEDUPDATE)) :-:
       			 
       ("inlnot",	 (P.INLNOT)) :-:
       ("floor",         (P.ROUND{floor=true,
                               fromkind=P.FLOAT 64,
                               tokind=P.INT 31})) :-:
       ("round",         (P.ROUND{floor=false, 
                               fromkind=P.FLOAT 64,
                               tokind=P.INT 31})) :-:
       ("real",          (P.REAL{fromkind=P.INT 31,
                               tokind=P.FLOAT 64})) :-:
       ("real32",        (P.REAL{fromkind=P.INT 32,
			         tokind=P.FLOAT 64})) :-:
       			 
       ("ordof",         (P.NUMSUBSCRIPT{kind=P.INT 8,
                                      checked=false,
                                      immutable=true})) :-:
       ("store",         (P.NUMUPDATE{kind=P.INT 8,
                                   checked=false})) :-:
       ("inlbyteof",     (P.NUMSUBSCRIPT{kind=P.INT 8,
                                      checked=true,
                                      immutable=false})) :-:
       ("inlstore",      (P.NUMUPDATE{kind=P.INT 8,
                                      checked=true})) :-:
       ("inlordof",      (P.NUMSUBSCRIPT{kind=P.INT 8,
                                      checked=true,
                                      immutable=true})) :-:

       (*** polymorphic array and vector ***)
       ("mkarray",       (P.INLMKARRAY)) :-:
       ("arrSub", 	 (P.SUBSCRIPT)) :-:
       ("arrChkSub",	 (P.INLSUBSCRIPT)) :-:
       ("vecSub",	 (P.SUBSCRIPTV)) :-:
       ("vecChkSub",	 (P.INLSUBSCRIPTV)) :-:
       ("arrUpdate",	 (P.UPDATE)) :-:
       ("arrChkUpdate",  (P.INLUPDATE)) :-:

       (* new array representations *)
	("newArray0",	 (P.NEW_ARRAY0)) :-:
	("getSeqData",	 (P.GET_SEQ_DATA)) :-:
	("recordSub",	 (P.SUBSCRIPT_REC)) :-:
	("raw64Sub",	 (P.SUBSCRIPT_RAW64)) :-:

       (* *** conversion primops ***
	*   There are certain duplicates for the same primop (but with
	*   different types).  In such a case, the "canonical" name
	*   of the primop has been extended using a simple suffix
	*   scheme. *)
       ("test_32_31_w",  (P.TEST(32,31))) :-:
       ("test_32_31_i",  (P.TEST(32,31))) :-:

       ("testu_31_31",   (P.TESTU(31,31))) :-:

       ("testu_32_31",   (P.TESTU(32,31))) :-:

       ("testu_32_32",   (P.TESTU(32,32))) :-:

       ("copy_32_32_ii", (P.COPY(32,32))) :-:
       ("copy_32_32_wi", (P.COPY(32,32))) :-:
       ("copy_32_32_iw", (P.COPY(32,32))) :-:
       ("copy_32_32_ww", (P.COPY(32,32))) :-:

       ("copy_31_31_ii", (P.COPY(31,31))) :-:
       ("copy_31_31_wi", (P.COPY(31,31))) :-:
       ("copy_31_31_iw", (P.COPY(31,31))) :-:

       ("copy_31_32_i",  (P.COPY(31,32))) :-:
       ("copy_31_32_w",  (P.COPY(31,32))) :-:

       ("copy_8_32_i",   (P.COPY(8,32))) :-:
       ("copy_8_32_w",   (P.COPY(8,32))) :-:

       ("copy_8_31",     (P.COPY(8,31))) :-:

       ("extend_31_32_ii", (P.EXTEND(31,32))) :-:
       ("extend_31_32_iw", (P.EXTEND(31,32))) :-:
       ("extend_31_32_wi", (P.EXTEND(31,32))) :-:
       ("extend_31_32_ww", (P.EXTEND(31,32))) :-:

       ("extend_8_31",   (P.EXTEND(8,31))) :-:

       ("extend_8_32_i", (P.EXTEND(8,32))) :-:
       ("extend_8_32_w", (P.EXTEND(8,32))) :-:

       ("trunc_32_31_i", (P.TRUNC(32,31))) :-:
       ("trunc_32_31_w", (P.TRUNC(32,31))) :-:

       ("trunc_31_8",    (P.TRUNC(31,8))) :-:

       ("trunc_32_8_i",  (P.TRUNC(32,8))) :-:
       ("trunc_32_8_w",  (P.TRUNC(32,8))) :-:

       (* conversion primops involving intinf *)
       ("test_inf_31",   (P.TEST_INF 31)) :-:
       ("test_inf_32",   (P.TEST_INF 32)) :-:
       ("test_inf_64",   (P.TEST_INF 64)) :-:
       ("copy_8_inf",    (P.COPY_INF 8)) :-:
       ("copy_8_inf_w",  (P.COPY_INF 8)) :-:
       ("copy_31_inf_w", (P.COPY_INF 31)) :-:
       ("copy_32_inf_w", (P.COPY_INF 32)) :-:
       ("copy_64_inf_w", (P.COPY_INF 64)) :-:
       ("copy_31_inf_i", (P.COPY_INF 31)) :-:
       ("copy_32_inf_i", (P.COPY_INF 32)) :-:
       ("copy_64_inf_i", (P.COPY_INF 64)) :-:
       ("extend_8_inf",  (P.EXTEND_INF 8)) :-:
       ("extend_8_inf_w",  (P.EXTEND_INF 8)) :-:
       ("extend_31_inf_w", (P.EXTEND_INF 31)) :-:
       ("extend_32_inf_w", (P.EXTEND_INF 32)) :-:
       ("extend_64_inf_w", (P.EXTEND_INF 64)) :-:
       ("extend_31_inf_i", (P.EXTEND_INF 31)) :-:
       ("extend_32_inf_i", (P.EXTEND_INF 32)) :-:
       ("extend_64_inf_i", (P.EXTEND_INF 64)) :-:
       ("trunc_inf_8",   (P.TRUNC_INF 8)) :-:
       ("trunc_inf_31",  (P.TRUNC_INF 31)) :-:
       ("trunc_inf_32",  (P.TRUNC_INF 32)) :-:
       ("trunc_inf_64",  (P.TRUNC_INF 64)) :-:
       
       (* primops to go between abstract and concrete representation of
	* 64-bit ints and words *)
       ("w64p",          (P.CVT64)) :-:
       ("p64w",          (P.CVT64)) :-:
       ("i64p",          (P.CVT64)) :-:
       ("p64i",          (P.CVT64)) :-:

       (* *** integer 31 primops ***
        *   Many of the i31 primops are being abused for different types
	*   (mostly Word8.word and also for char).  In these cases
	*   there are suffixed alternative versions of the primop
	*   (i.e., same primop, different type). *)
       ("i31add", 	 (int31 P.+)) :-:
       ("i31add_8", 	 (int31 P.+)) :-:

       ("i31sub",	 (int31 P.-)) :-:
       ("i31sub_8",	 (int31 P.-)) :-:

       ("i31mul",	 (int31 P.* )) :-:
       ("i31mul_8",	 (int31 P.* )) :-:

       ("i31div",	 (int31 P.DIV)) :-:
       ("i31div_8",	 (int31 P.DIV)) :-:

       ("i31mod",        (int31 P.MOD)) :-:
       ("i31mod_8",      (int31 P.MOD)) :-:

       ("i31quot",	 (int31 P./)) :-:

       ("i31rem",	 (int31 P.REM)) :-:

       ("i31orb",	 (bits31 P.ORB)) :-:
       ("i31orb_8",	 (bits31 P.ORB)) :-:

       ("i31andb",	 (bits31 P.ANDB)) :-:
       ("i31andb_8",	 (bits31 P.ANDB)) :-:

       ("i31xorb",	 (bits31 P.XORB)) :-:
       ("i31xorb_8",	 (bits31 P.XORB)) :-:

       ("i31notb",	 (bits31 P.NOTB)) :-:
       ("i31notb_8",	 (bits31 P.NOTB)) :-:

       ("i31neg",	 (int31 P.~)) :-:
       ("i31neg_8",	 (int31 P.~)) :-:

       ("i31lshift",	 (bits31 P.LSHIFT)) :-:
       ("i31lshift_8",	 (bits31 P.LSHIFT)) :-:

       ("i31rshift",	 (bits31 P.RSHIFT)) :-:
       ("i31rshift_8",	 (bits31 P.RSHIFT)) :-:

       ("i31lt",	 (int31cmp P.<)) :-:
       ("i31lt_8",	 (int31cmp P.<)) :-:
       ("i31lt_c",	 (int31cmp P.<)) :-:

       ("i31le",	 (int31cmp P.<=)) :-:
       ("i31le_8",	 (int31cmp P.<=)) :-:
       ("i31le_c",	 (int31cmp P.<=)) :-:

       ("i31gt",	 (int31cmp P.>)) :-:
       ("i31gt_8",	 (int31cmp P.>)) :-:
       ("i31gt_c",	 (int31cmp P.>)) :-:

       ("i31ge", 	 (int31cmp P.>=)) :-:
       ("i31ge_8", 	 (int31cmp P.>=)) :-:
       ("i31ge_c", 	 (int31cmp P.>=)) :-:

       ("i31ltu",	 (word31cmp P.LTU)) :-:
       ("i31geu",	 (word31cmp P.GEU)) :-:
       ("i31eq",	 (int31cmp P.EQL)) :-:
       ("i31ne",	 (int31cmp P.NEQ)) :-:

       ("i31min",	 (P.INLMIN (P.INT 31))) :-:
       ("i31min_8",	 (P.INLMIN (P.INT 31))) :-:
       ("i31max",	 (P.INLMAX (P.INT 31))) :-:
       ("i31max_8",	 (P.INLMAX (P.INT 31))) :-:

       ("i31abs",	 (P.INLABS (P.INT 31))) :-:

       (*** integer 32 primops ***)
       ("i32mul",        (int32 P.* )) :-:
       ("i32div",        (int32 P.DIV)) :-:
       ("i32mod",        (int32 P.MOD)) :-:
       ("i32quot",       (int32 P./)) :-:
       ("i32rem",        (int32 P.REM)) :-:
       ("i32add",        (int32 P.+)) :-:
       ("i32sub",        (int32 P.-)) :-:
       ("i32orb",        (bits32 P.ORB)) :-:
       ("i32andb",       (bits32 P.ANDB)) :-:
       ("i32xorb",       (bits32 P.XORB)) :-:
       ("i32lshift",     (bits32 P.LSHIFT)) :-:
       ("i32rshift",     (bits32 P.RSHIFT)) :-:
       ("i32neg",        (int32 P.~)) :-:
       ("i32lt",         (int32cmp P.<)) :-:
       ("i32le",         (int32cmp P.<=)) :-:
       ("i32gt",         (int32cmp P.>)) :-:
       ("i32ge",         (int32cmp P.>=)) :-:
       ("i32eq",         (int32cmp P.EQL)) :-:
       ("i32ne",         (int32cmp P.NEQ)) :-:

       ("i32min",	 (P.INLMIN (P.INT 32))) :-:
       ("i32max",	 (P.INLMAX (P.INT 32))) :-:
       ("i32abs",	 (P.INLABS (P.INT 32))) :-:

       (*** float 64 primops ***)
       ("f64add", 	 (purefloat64 (P.+))) :-:
       ("f64sub",	 (purefloat64 (P.-))) :-:
       ("f64div", 	 (purefloat64 (P./))) :-:
       ("f64mul",	 (purefloat64 (P.* ))) :-:
       ("f64neg",	 (purefloat64 P.~)) :-:
       ("f64ge",	 (float64cmp (P.>=))) :-:
       ("f64gt",	 (float64cmp (P.>))) :-:
       ("f64le",	 (float64cmp (P.<=))) :-:
       ("f64lt",	 (float64cmp (P.<))) :-:
       ("f64eq",	 (float64cmp P.EQL)) :-:
       ("f64ne",	 (float64cmp P.NEQ)) :-:
       ("f64abs",	 (purefloat64 P.ABS)) :-:

       ("f64sin",	 (purefloat64 P.FSIN)) :-:
       ("f64cos",	 (purefloat64 P.FCOS)) :-:
       ("f64tan",	 (purefloat64 P.FTAN)) :-:
       ("f64sqrt",	 (purefloat64 P.FSQRT)) :-:

       ("f64min",	 (P.INLMIN (P.FLOAT 64))) :-:
       ("f64max",	 (P.INLMAX (P.FLOAT 64))) :-:

       (*** float64 array ***)	
       ("f64Sub",	 (sub (P.FLOAT 64))) :-:
       ("f64chkSub",	 (chkSub (P.FLOAT 64))) :-:
       ("f64Update",	 (update (P.FLOAT 64))) :-:
       ("f64chkUpdate",  (chkUpdate (P.FLOAT 64))) :-:

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
        * ("w8mul",	word8 (P.* ))) :-:
	* ("w8div",	word8 (P./))) :-:
	* ("w8add",	word8 (P.+))) :-:
	* ("w8sub",	word8 (P.-))) :-:
        *		
        * ("w8notb",	word31 P.NOTB)) :-:
	* ("w8rshift",	word8 P.RSHIFT)) :-:
	* ("w8rshiftl",	word8 P.RSHIFTL)) :-:
	* ("w8lshift",	word8 P.LSHIFT)) :-:
        *
	* ("w8toint",   P.ROUND{floor=true, 
        *                     fromkind=P.UINT 8, 
        *                     tokind=P.INT 31})) :-:
	* ("w8fromint", P.REAL{fromkind=P.INT 31,
        *                    tokind=P.UINT 8})) :-:
        *)
  
       ("w8orb",	(word31 P.ORB)) :-:
       ("w8xorb",	(word31 P.XORB)) :-:
       ("w8andb",	(word31 P.ANDB)) :-:
       	 		
       ("w8gt",	        (word8cmp P.>)) :-:
       ("w8ge",	        (word8cmp P.>=)) :-:
       ("w8lt",	        (word8cmp P.<)) :-:
       ("w8le",		(word8cmp P.<=)) :-:
       ("w8eq",		(word8cmp P.EQL)) :-:
       ("w8ne",		(word8cmp P.NEQ)) :-:

       (*** word8 array and vector ***)
       ("w8Sub",	(sub (P.UINT 8))) :-:
       ("w8chkSub",	(chkSub (P.UINT 8))) :-:
       ("w8subv",	(subv (P.UINT 8))) :-:
       ("w8chkSubv",	(chkSubv (P.UINT 8))) :-:
       ("w8update",	(update (P.UINT 8))) :-:
       ("w8chkUpdate",  (chkUpdate (P.UINT 8))) :-:

       (* word31 primops *)
       ("w31mul",	(word31 (P.* ))) :-:
       ("w31div",	(word31 (P./))) :-:
       ("w31mod",	(word31 (P.REM))) :-:
       ("w31add",	(word31 (P.+))) :-:
       ("w31sub",	(word31 (P.-))) :-:
       ("w31orb",	(word31 P.ORB)) :-:
       ("w31xorb",	(word31 P.XORB)) :-:
       ("w31andb",	(word31 P.ANDB)) :-:
       ("w31notb",	(word31 P.NOTB)) :-:
       ("w31neg",       (word31 P.~)) :-:
       ("w31rshift",	(word31 P.RSHIFT)) :-:
       ("w31rshiftl",   (word31 P.RSHIFTL)) :-:
       ("w31lshift",	(word31 P.LSHIFT)) :-:
       ("w31gt",	(word31cmp (P.>))) :-:
       ("w31ge",	(word31cmp (P.>=))) :-:
       ("w31lt",	(word31cmp (P.<))) :-:
       ("w31le",	(word31cmp (P.<=))) :-:
       ("w31eq",	(word31cmp P.EQL)) :-:
       ("w31ne",	(word31cmp P.NEQ)) :-:
       ("w31ChkRshift", (P.INLRSHIFT(P.UINT 31))) :-:
       ("w31ChkRshiftl",(P.INLRSHIFTL(P.UINT 31))) :-:
       ("w31ChkLshift", (P.INLLSHIFT(P.UINT 31))) :-:

       ("w31min",	(P.INLMIN (P.UINT 31))) :-:
       ("w31max",	(P.INLMAX (P.UINT 31))) :-:
       
       (* (pseudo-)word8 primops *)
       ("w31mul_8",	(word31 (P.* ))) :-:
       ("w31div_8",	(word31 (P./))) :-:
       ("w31mod_8",	(word31 (P.REM))) :-:
       ("w31add_8",	(word31 (P.+))) :-:
       ("w31sub_8",	(word31 (P.-))) :-:
       ("w31orb_8",	(word31 P.ORB)) :-:
       ("w31xorb_8",	(word31 P.XORB)) :-:
       ("w31andb_8",	(word31 P.ANDB)) :-:
       ("w31notb_8",	(word31 P.NOTB)) :-:
       ("w31neg_8",     (word31 P.~)) :-:
       ("w31rshift_8",	(word31 P.RSHIFT)) :-:
       ("w31rshiftl_8", (word31 P.RSHIFTL)) :-:
       ("w31lshift_8",	(word31 P.LSHIFT)) :-:
       ("w31gt_8",	(word31cmp (P.>))) :-:
       ("w31ge_8",	(word31cmp (P.>=))) :-:
       ("w31lt_8",	(word31cmp (P.<))) :-:
       ("w31le_8",	(word31cmp (P.<=))) :-:
       ("w31eq_8",	(word31cmp P.EQL)) :-:
       ("w31ne_8",	(word31cmp P.NEQ)) :-:
       ("w31ChkRshift_8", (P.INLRSHIFT(P.UINT 31))) :-:
       ("w31ChkRshiftl_8",(P.INLRSHIFTL(P.UINT 31))) :-:
       ("w31ChkLshift_8", (P.INLLSHIFT(P.UINT 31))) :-:

       ("w31min_8",	(P.INLMIN (P.UINT 31))) :-:
       ("w31max_8",	(P.INLMAX (P.UINT 31))) :-:

       (*** word32 primops ***)
       ("w32mul",	(word32 (P.* ))) :-:
       ("w32div",	(word32 (P./))) :-:
       ("w32mod",	(word32 (P.REM))) :-:
       ("w32add",	(word32 (P.+))) :-:
       ("w32sub",	(word32 (P.-))) :-:
       ("w32orb",	(word32 P.ORB)) :-:
       ("w32xorb",	(word32 P.XORB)) :-:
       ("w32andb",	(word32 P.ANDB)) :-:
       ("w32notb",	(word32 P.NOTB)) :-:
       ("w32neg",	(word32 P.~)) :-:
       ("w32rshift",	(word32 P.RSHIFT)) :-:
       ("w32rshiftl",   (word32 P.RSHIFTL)) :-:
       ("w32lshift",	(word32 P.LSHIFT)) :-:
       ("w32gt",	(word32cmp (P.>))) :-:
       ("w32ge",	(word32cmp (P.>=))) :-:
       ("w32lt",	(word32cmp (P.<))) :-:
       ("w32le",	(word32cmp (P.<=))) :-:
       ("w32eq",	(word32cmp P.EQL)) :-:
       ("w32ne",	(word32cmp P.NEQ)) :-:
       ("w32ChkRshift", (P.INLRSHIFT(P.UINT 32))) :-:
       ("w32ChkRshiftl",(P.INLRSHIFTL(P.UINT 32))) :-:
       ("w32ChkLshift", (P.INLLSHIFT(P.UINT 32))) :-:

       ("w32min",	(P.INLMIN (P.UINT 32))) :-:
       ("w32max",	(P.INLMAX (P.UINT 32))) :-:

       (* experimental C FFI primops *)
       ("raww8l",       (P.RAW_LOAD (P.UINT 8))) :-:
       ("rawi8l",       (P.RAW_LOAD (P.INT 8))) :-:
       ("raww16l",      (P.RAW_LOAD (P.UINT 16))) :-:
       ("rawi16l",      (P.RAW_LOAD (P.INT 16))) :-:
       ("raww32l",      (P.RAW_LOAD (P.UINT 32))) :-:
       ("rawi32l",      (P.RAW_LOAD (P.INT 32))) :-:
       ("rawf32l",      (P.RAW_LOAD (P.FLOAT 32))) :-:
       ("rawf64l",      (P.RAW_LOAD (P.FLOAT 64))) :-:
       ("raww8s",       (P.RAW_STORE (P.UINT 8))) :-:
       ("rawi8s",       (P.RAW_STORE (P.INT 8))) :-:
       ("raww16s",      (P.RAW_STORE (P.UINT 16))) :-:
       ("rawi16s",      (P.RAW_STORE (P.INT 16))) :-:
       ("raww32s",      (P.RAW_STORE (P.UINT 32))) :-:
       ("rawi32s",      (P.RAW_STORE (P.INT 32))) :-:
       ("rawf32s",      (P.RAW_STORE (P.FLOAT 32))) :-:
       ("rawf64s",      (P.RAW_STORE (P.FLOAT 64))) :-:
       ("rawccall",     (P.RAW_CCALL NONE)) :-:

          (* Support for direct construction of C objects on ML heap.
           * rawrecord builds a record holding C objects on the heap.
           * rawselectxxx index on this record.  They are of type:
           *    'a * Word32.word -> Word32.word
           * The 'a is to guarantee that the compiler will treat
           * the record as a ML object, in case it passes thru a gc boundary.
           * rawupdatexxx writes to the record.
           *) 

       ("rawrecord",    (P.RAW_RECORD { fblock = false })) :-:
       ("rawrecord64",  (P.RAW_RECORD { fblock = true })) :-:

       ("rawselectw8",  (P.RAW_LOAD (P.UINT 8))) :-:
       ("rawselecti8",  (P.RAW_LOAD (P.INT 8))) :-:
       ("rawselectw16", (P.RAW_LOAD (P.UINT 16))) :-:
       ("rawselecti16", (P.RAW_LOAD (P.INT 16))) :-:
       ("rawselectw32", (P.RAW_LOAD (P.UINT 32))) :-:
       ("rawselecti32", (P.RAW_LOAD (P.INT 32))) :-:
       ("rawselectf32", (P.RAW_LOAD (P.FLOAT 32))) :-:
       ("rawselectf64", (P.RAW_LOAD (P.FLOAT 64))) :-:

       ("rawupdatew8",  (P.RAW_STORE (P.UINT 8))) :-:
       ("rawupdatei8",  (P.RAW_STORE (P.INT 8))) :-:
       ("rawupdatew16", (P.RAW_STORE (P.UINT 16))) :-:
       ("rawupdatei16", (P.RAW_STORE (P.INT 16))) :-:
       ("rawupdatew32", (P.RAW_STORE (P.UINT 32))) :-:
       ("rawupdatei32", (P.RAW_STORE (P.INT 32))) :-:
       ("rawupdatef32", (P.RAW_STORE (P.FLOAT 32))) :-:
       ("rawupdatef64", (P.RAW_STORE (P.FLOAT 64)))


fun primopMap name = RBMap.find(primops,name)

end (* structure PrimOpMap *)
