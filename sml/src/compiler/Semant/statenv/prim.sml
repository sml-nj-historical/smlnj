(* Copyright 1996 by AT&T Bell Laboratories *)
(* prim.sml *)

signature PRIM_ENV = 
sig 

  val primEnv : StaticEnv.staticEnv

end (* signature PRIM_ENV *)


structure PrimEnv : PRIM_ENV = 
struct

local
  structure S = Symbol
  structure M = Modules
  structure B = Bindings
  structure SP = SymPath
  structure IP = InvPath
  structure SE = StaticEnv
  structure EE = EntityEnv

  structure BT = BasicTypes
  structure T = Types
  structure TU = TypesUtil
  structure MU = ModuleUtil
  structure P = PrimOp

  structure ST = Stamps
  structure V = VarCon

  structure A = Access
  structure II = InlInfo

in

fun mkTycElement (name: string, tyc) = 
     (S.tycSymbol name, M.TYCspec{entVar=ST.special name, spec=tyc, repl=false,
				  scope=0})

(* 
 * Note: this function only applies to constructors but not exceptions;
 * exceptions will have a non-trivial slot number 
 *)
fun mkConElement (name, d) = 
    (S.varSymbol name, M.CONspec{spec=d, slot=NONE})

(* primTypes structure *)
val primTypes =
  let val primTycs =
            [("bool", BT.boolTycon),
             ("list", BT.listTycon),
             ("ref", BT.refTycon),
             ("unit", BT.unitTycon),
             ("int", BT.intTycon),
             ("int32", BT.int32Tycon),
             ("real", BT.realTycon),
             ("word", BT.wordTycon),
             ("word8", BT.word8Tycon),
             ("word32", BT.word32Tycon),
             ("cont", BT.contTycon),
             ("control_cont", BT.ccontTycon),
             ("array", BT.arrayTycon),
             ("vector", BT.vectorTycon),
             ("object", BT.objectTycon),
             ("c_function", BT.c_functionTycon),
             ("word8array", BT.word8arrayTycon),
             ("real64array", BT.real64arrayTycon),
             ("spin_lock", BT.spin_lockTycon),
             ("string", BT.stringTycon),
             ("char", BT.charTycon),
             ("exn", BT.exnTycon),
             ("frag", BT.fragTycon),
             ("susp", BT.suspTycon)]

      val primCons = 
            [("true", BT.trueDcon),
             ("false", BT.falseDcon),
             ("::", BT.consDcon),
             ("nil", BT.nilDcon),
             ("ref", BT.refDcon),
             ("QUOTE", BT.QUOTEDcon),
             ("ANTIQUOTE", BT.ANTIQUOTEDcon),
             ("$", BT.dollarDcon)]

      val tycElements = map mkTycElement primTycs
      val conElements = map mkConElement primCons
		 

      val allElements = tycElements@conElements
      val allSymbols = map #1 allElements

      val entities = foldr (fn ((_,M.TYCspec{spec,entVar,repl,scope}),r) =>
			         EE.bind(entVar,M.TYCent spec,r)) 
                          EE.empty tycElements

      val entities = EntityEnv.mark(fn _ => ST.special"primEntEnv", entities)

   in M.STR{sign=M.SIG{name=SOME(S.sigSymbol "PRIMTYPES"), closed=true,
		       fctflag=false, stamp=ST.special "PrimTypesSig",
                       symbols=allSymbols,elements=allElements,
		       typsharing=nil,strsharing=nil,
		       boundeps=ref (SOME []), lambdaty=ref(NONE)},
	    rlzn={stamp=ST.special "PrimTypesStr",
		  entities=entities,
		  lambdaty=ref NONE, 
		  rpath=IP.IPATH[S.strSymbol "primTypes"]},

	    access=A.nullAcc, info=II.mkStrInfo []}

  end (* primTypes *)


(**************************************************************************
 *                 BUILDING A COMPLETE LIST OF PRIMOPS                    *
 **************************************************************************)

local

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

fun float size oper = P.ARITH{oper=oper, overflow=true, kind=P.FLOAT size}
val float64 = float 64

fun purefloat size oper = P.ARITH{oper=oper,overflow=false,kind=P.FLOAT size}
val purefloat64 = purefloat 64	

fun cmp kind oper = P.CMP{oper=oper, kind=kind}
val int31cmp = cmp (P.INT 31)
val int32cmp = cmp (P.INT 32)

val word32cmp = cmp (P.UINT 32)
val word31cmp = cmp (P.UINT 31)
val word8cmp  = cmp (P.UINT 8)

val float64cmp = cmp (P.FLOAT 64)

val v1 = T.IBOUND 0
val v2 = T.IBOUND 1
val v3 = T.IBOUND 2

fun pa(t1,t2) =  BT.tupleTy [t1,t2]
fun tp(t1,t2,t3) =  BT.tupleTy [t1,t2,t3]
fun ar(t1,t2) = BT.--> (t1, t2)

fun ap(tc,l) = T.CONty(tc, l)
fun cnt t = T.CONty(BT.contTycon,[t])
fun ccnt t = T.CONty(BT.ccontTycon,[t])
fun rf t = T.CONty(BT.refTycon,[t])
fun ay t = T.CONty(BT.arrayTycon,[t])
fun vct t = T.CONty(BT.vectorTycon,[t])

val bo = BT.boolTy
val i = BT.intTy
val u = BT.unitTy

fun p0 t = SOME t
fun p1 t = SOME(T.POLYty {sign=[false], tyfun=T.TYFUN {arity=1, body=t}})
fun ep1 t = SOME(T.POLYty {sign=[true], tyfun=T.TYFUN {arity=1, body=t}})
fun p2 t = SOME(T.POLYty {sign=[false,false], 
                          tyfun=T.TYFUN {arity=2, body=t}})
fun p3 t = SOME(T.POLYty {sign=[false,false,false], 
                          tyfun=T.TYFUN {arity=3, body=t}})

fun sub kind = P.NUMSUBSCRIPT{kind=kind, checked=false, immutable=false}
fun chkSub kind = P.NUMSUBSCRIPT{kind=kind, checked=true, immutable=false}

fun subv kind = P.NUMSUBSCRIPT{kind=kind, checked=false, immutable=true}
fun chkSubv kind = P.NUMSUBSCRIPT{kind=kind, checked=true, immutable=true}

fun update kind = P.NUMUPDATE {kind=kind, checked=false}
fun chkUpdate kind = P.NUMUPDATE {kind=kind, checked=true}

val numSubTy = p2(ar(pa(v1,i),v2))
val numUpdTy = p2(ar(tp(v1,i,v2),u))

in

val allPrimops =
      [("callcc",	 P.CALLCC,     	p1(ar(ar(cnt(v1),v1),v1))),
       ("throw",	 P.THROW,      	p2(ar(cnt(v1),ar(v1,v2)))),
       ("capture",	 P.CAPTURE,     p1(ar(ar(ccnt(v1),v1),v1))),
       ("isolate",	 P.ISOLATE,     p1(ar(ar(v1,u),cnt(v1)))),
       ("cthrow",	 P.THROW,      	p2(ar(ccnt(v1),ar(v1,v2)))),
       ("!",		 P.DEREF,      	p1(ar(rf(v1),v1))),
       (":=",	         P.ASSIGN,      p1(ar(pa(rf(v1),v1),u))),
       ("makeref",	 P.MAKEREF,     p1(ar(v1,rf(v1)))),
       ("boxed",	 P.BOXED,      	p1(ar(v1,bo))),
       ("unboxed",	 P.UNBOXED,     p1(ar(v1,bo))),
       ("cast",	         P.CAST,      	p2(ar(v1,v2))),
       ("=",		 P.POLYEQL,     ep1(ar(pa(v1,v1),bo))),
       ("<>",	         P.POLYNEQ,     ep1(ar(pa(v1,v1),bo))),
       ("ptreql",	 P.PTREQL,      p1(ar(pa(v1,v1),bo))),
       ("ptrneq",	 P.PTRNEQ,      p1(ar(pa(v1,v1),bo))),
       ("getvar",	 P.GETVAR,      p1(ar(u,v1))),
       ("setvar",	 P.SETVAR,      p1(ar(v1,u))),
       ("setpseudo",	 P.SETPSEUDO,   p1(ar(pa(v1,i),u))),
       ("getpseudo",	 P.GETPSEUDO,   p1(ar(i,v1))),
       ("mkspecial",     P.MKSPECIAL,   p2(ar(pa(i,v1),v2))),
       ("getspecial",    P.GETSPECIAL,  p1(ar(v1,i))),
       ("setspecial",    P.SETSPECIAL,  p1(ar(pa(v1,i),u))),
       ("gethdlr",	 P.GETHDLR,     p1(ar(u,cnt(v1)))),
       ("sethdlr",	 P.SETHDLR,     p1(ar(cnt(v1),u))),
       ("gettag", 	 P.GETTAG,      p1(ar(v1,i))),
       ("setmark",	 P.SETMARK,     p1(ar(v1,u))),
       ("dispose",	 P.DISPOSE,     p1(ar(v1,u))),
       ("compose",	 P.INLCOMPOSE,  p3(ar(pa(ar(v2,v3),ar(v1,v2)),ar(v1,v3)))),
       ("before",	 P.INLBEFORE,   p2(ar(pa(v1,v2),v1))),
			 
       			 
       ("length",	 P.LENGTH,     	p1(ar(v1,i))),
       ("objlength",	 P.OBJLENGTH,   p1(ar(v1,i))),

       (*  
        * I believe the following five primops should not be exported into
        * the InLine structure. (ZHONG) 
        *)
       ("boxedupdate",   P.BOXEDUPDATE,   NONE),
       ("unboxedupdate", P.UNBOXEDUPDATE, NONE),
       ("getrunvec",	 P.GETRUNVEC,     NONE),
       ("uselvar",	 P.USELVAR,       NONE),
       ("deflvar",	 P.DEFLVAR,       NONE),
       			 
       ("inlnot",	 P.INLNOT,      	NONE),
       ("floor",         P.ROUND{floor=true,
                               fromkind=P.FLOAT 64,
                               tokind=P.INT 31},      	NONE),
       ("round",         P.ROUND{floor=false, 
                               fromkind=P.FLOAT 64,
                               tokind=P.INT 31},      	NONE),
       ("real",          P.REAL{fromkind=P.INT 31,
                              tokind=P.FLOAT 64},      	NONE),
       			 
       ("ordof",         P.NUMSUBSCRIPT{kind=P.INT 8,
                                      checked=false,
                                      immutable=true},  numSubTy),
       ("store",         P.NUMUPDATE{kind=P.INT 8,
                                   checked=false},      numUpdTy),
       ("inlbyteof",     P.NUMSUBSCRIPT{kind=P.INT 8,
                                      checked=true,
                                      immutable=false}, numSubTy),
       ("inlstore",      P.NUMUPDATE{kind=P.INT 8,
                                   checked=true},      	numUpdTy),
       ("inlordof",      P.NUMSUBSCRIPT{kind=P.INT 8,
                                      checked=true,
                                      immutable=true},  numSubTy),

       (*** polymorphic array and vector ***)
       ("mkarray",       P.INLMKARRAY,          p1(ar(pa(i,v1),ay(v1)))),
       ("arrSub", 	 P.SUBSCRIPT,      	p1(ar(pa(ay(v1),i),v1))),
       ("arrChkSub",	 P.INLSUBSCRIPT,      	p1(ar(pa(ay(v1),i),v1))),
       ("vecSub",	 P.SUBSCRIPTV,      	p1(ar(pa(vct(v1),i),v1))),
       ("vecChkSub",	 P.INLSUBSCRIPTV,      	p1(ar(pa(vct(v1),i),v1))),
       ("arrUpdate",	 P.UPDATE,      	p1(ar(tp(ay(v1),i,v1),u))),
       ("arrChkUpdate",  P.INLUPDATE,      	p1(ar(tp(ay(v1),i,v1),u))),

       (* conversion primops *)
       ("test_32_31",    P.TEST(32,31),  	NONE),
       ("testu_31_31",   P.TESTU(31,31),        NONE),
       ("testu_32_31",   P.TESTU(32,31),        NONE),
       ("testu_32_32",   P.TESTU(32,32),   	NONE),
       ("copy_32_32",    P.COPY(32,32),   	NONE),
       ("copy_31_31",    P.COPY(31,31),   	NONE),
       ("copy_31_32",    P.COPY(31,32),   	NONE),
       ("copy_8_32",     P.COPY(8,32),     	NONE),
       ("copy_8_31",     P.COPY(8,31),     	NONE),
       ("extend_31_32",  P.EXTEND(31,32), 	NONE),
       ("extend_8_31",   P.EXTEND(8,31),   	NONE),
       ("extend_8_32",   P.EXTEND(8,32), 	NONE),
       ("trunc_32_31",   P.TRUNC(32,31),   	NONE),
       ("trunc_31_8",    P.TRUNC(31,8),   	NONE),
       ("trunc_32_8",    P.TRUNC(32,8),   	NONE),
       
       (*** integer 31 primops ***)
       ("i31mul",	 int31 (P.* ),      	NONE),
       ("i31quot",	 int31 (P./),      	NONE),
       ("i31add", 	 int31 (P.+),      	NONE),
       ("i31sub",	 int31 (P.-),      	NONE),
       ("i31orb",	 bits31 P.ORB,      	NONE),
       ("i31andb",	 bits31 P.ANDB,      	NONE),
       ("i31xorb",	 bits31 P.XORB,      	NONE),
       ("i31notb",	 bits31 P.NOTB,      	NONE),
       ("i31neg",	 int31 P.~,      	NONE),
       ("i31lshift",	 bits31 P.LSHIFT,      	NONE),
       ("i31rshift",	 bits31 P.RSHIFT,      	NONE),
       ("i31lt",	 int31cmp (P.<),     NONE),
       ("i31le",	 int31cmp (P.<=),    NONE),
       ("i31gt",	 int31cmp (P.>),     NONE),
       ("i31ge", 	 int31cmp (P.>=),    NONE),
       ("i31ltu",	 word31cmp P.LTU,      	NONE), 
       ("i31geu",	 word31cmp P.GEU,      	NONE),
       ("i31mod",        P.INLMOD,      	NONE),
       ("i31div",	 P.INLDIV,      	NONE),
       ("i31rem",	 P.INLREM,      	NONE),
       ("i31max",	 P.INLMAX,      	NONE),
       ("i31min",	 P.INLMIN,      	NONE),
       ("i31abs",	 P.INLABS,      	NONE),
       ("i31eq",	 int31cmp P.EQL,      	NONE),
       ("i31ne",	 int31cmp P.NEQ,      	NONE),

       (*** integer 32 primops ***)
       ("i32mul",        int32 (P.* ),      	NONE),
       ("i32quot",       int32 (P./),      	NONE),
       ("i32add",        int32 (P.+),      	NONE),
       ("i32sub",        int32 (P.-),      	NONE),
       ("i32orb",        bits32 P.ORB,      	NONE),
       ("i32andb",       bits32 P.ANDB,      	NONE),
       ("i32xorb",       bits32 P.XORB,      	NONE),
       ("i32lshift",     bits32 P.LSHIFT,      	NONE),
       ("i32rshift",     bits32 P.RSHIFT,      	NONE),
       ("i32neg",        int32 P.~,      	NONE),
       ("i32lt",         int32cmp (P.<),     NONE),
       ("i32le",         int32cmp (P.<=),    NONE),
       ("i32gt",         int32cmp (P.>),     NONE),
       ("i32ge",         int32cmp (P.>=),    NONE),
       ("i32eq",         int32cmp (P.EQL),   NONE),
       ("i32ne",         int32cmp (P.NEQ),   NONE),

       (*
        * WARNING: the lambda types in translate.sml are all wrong for  
        * this. The inlprimops for these must be parameterized over the 
        * integer kind. 
        *
        *
        * ("i32mod",    P.INLMOD,      	NONE),
        * ("i32div",    P.INLDIV,      	NONE),
        * ("i32rem",    P.INLREM,      	NONE),
        * ("i32max",    P.INLMAX,      	NONE),
        * ("i32min",    P.INLMIN,      	NONE),
        * ("i32abs",    P.INLABS,      	NONE),
        *
        *)

       (*** float 64 primops ***)
       ("f64add", 	 float64 (P.+),      NONE),
       ("f64sub",	 float64 (P.-),      NONE),
       ("f64div", 	 float64 (P./),      NONE),
       ("f64mul",	 float64 (P.* ),     NONE),
       ("f64neg",	 purefloat64 P.~,      	NONE),
       ("f64ge",	 float64cmp (P.>=),  NONE),
       ("f64gt",	 float64cmp (P.>),   NONE),
       ("f64le",	 float64cmp (P.<=),  NONE),
       ("f64lt",	 float64cmp (P.<),   NONE),
       ("f64eq",	 float64cmp P.EQL,      NONE),
       ("f64ne",	 float64cmp P.NEQ,      NONE),
       ("f64abs",	 purefloat64 P.ABS,     NONE),

       (*** float64 array ***)	
       ("f64Sub",	 sub (P.FLOAT 64),        numSubTy),
       ("f64chkSub",	 chkSub (P.FLOAT 64),     numSubTy),
       ("f64Update",	 update (P.FLOAT 64),     numUpdTy),
       ("f64chkUpdate",  chkUpdate (P.FLOAT 64),  numUpdTy),

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
        * ("w8mul",	word8 (P.* ),      	NONE),
	* ("w8div",	word8 (P./),      	NONE),
	* ("w8add",	word8 (P.+),      	NONE),
	* ("w8sub",	word8 (P.-),      	NONE),
        *		
        * ("w8notb",	word31 P.NOTB,      	NONE),
	* ("w8rshift",	word8 P.RSHIFT,      	NONE),
	* ("w8rshiftl",	word8 P.RSHIFTL,      	NONE),
	* ("w8lshift",	word8 P.LSHIFT,      	NONE),
        *
	* ("w8toint",   P.ROUND{floor=true, 
        *                     fromkind=P.UINT 8, 
        *                     tokind=P.INT 31},   NONE),
	* ("w8fromint", P.REAL{fromkind=P.INT 31,
        *                    tokind=P.UINT 8},    NONE),
        *)
  
       ("w8orb",	word31 P.ORB,      	NONE),
       ("w8xorb",	word31 P.XORB,      	NONE),
       ("w8andb",	word31 P.ANDB,      	NONE),
       	 		
       ("w8gt",	        word8cmp P.>,         NONE),
       ("w8ge",	        word8cmp P.>=,        NONE),
       ("w8lt",	        word8cmp P.<,         NONE),
       ("w8le",		word8cmp P.<=,        NONE),
       ("w8eq",		word8cmp P.EQL,      	NONE),
       ("w8ne",		word8cmp P.NEQ,      	NONE),

       (*** word8 array and vector ***)
       ("w8Sub",	sub (P.UINT 8),      	numSubTy),
       ("w8chkSub",	chkSub (P.UINT 8),    numSubTy),
       ("w8subv",	subv (P.UINT 8),      numSubTy),
       ("w8chkSubv",	chkSubv (P.UINT 8),   numSubTy),
       ("w8update",	update (P.UINT 8),    numUpdTy),
       ("w8chkUpdate",  chkUpdate (P.UINT 8), numUpdTy),

       (* word31 primops *)
       ("w31mul",	word31 (P.* ),      	NONE),
       ("w31div",	word31 (P./),      	NONE),
       ("w31add",	word31 (P.+),      	NONE),
       ("w31sub",	word31 (P.-),      	NONE),
       ("w31orb",	word31 P.ORB,      	NONE),
       ("w31xorb",	word31 P.XORB,      	NONE),
       ("w31andb",	word31 P.ANDB,      	NONE),
       ("w31notb",	word31 P.NOTB,      	NONE),
       ("w31rshift",	word31 P.RSHIFT,      NONE),
       ("w31rshiftl",   word31 P.RSHIFTL,     NONE),
       ("w31lshift",	word31 P.LSHIFT,      NONE),
       ("w31gt",	word31cmp (P.>),      NONE),
       ("w31ge",	word31cmp (P.>=),     NONE),
       ("w31lt",	word31cmp (P.<),      NONE),
       ("w31le",	word31cmp (P.<=),     NONE),
       ("w31eq",	word31cmp P.EQL,      NONE),
       ("w31ne",	word31cmp P.NEQ,      NONE),
       ("w31ChkRshift", P.INLRSHIFT(P.UINT 31),    NONE),
       ("w31ChkRshiftl",P.INLRSHIFTL(P.UINT 31),   NONE),
       ("w31ChkLshift", P.INLLSHIFT(P.UINT 31),    NONE),
       
       (*** word32 primops ***)
       ("w32mul",	word32 (P.* ),      	NONE),
       ("w32div",	word32 (P./),      	NONE),
       ("w32add",	word32 (P.+),      	NONE),
       ("w32sub",	word32 (P.-),      	NONE),
       ("w32orb",	word32 P.ORB,      	NONE),
       ("w32xorb",	word32 P.XORB,      	NONE),
       ("w32andb",	word32 P.ANDB,      	NONE),
       ("w32notb",	word32 P.NOTB,      	NONE),
       ("w32rshift",	word32 P.RSHIFT,     	NONE),
       ("w32rshiftl",   word32 P.RSHIFTL,    	NONE),
       ("w32lshift",	word32 P.LSHIFT,     	NONE),
       ("w32gt",	word32cmp (P.>),      NONE),
       ("w32ge",	word32cmp (P.>=),     NONE),
       ("w32lt",	word32cmp (P.<),      NONE),
       ("w32le",	word32cmp (P.<=),     NONE),
       ("w32eq",	word32cmp P.EQL,     	NONE),
       ("w32ne",	word32cmp P.NEQ,     	NONE),
       ("w32ChkRshift", P.INLRSHIFT(P.UINT 32), NONE),
       ("w32ChkRshiftl",P.INLRSHIFTL(P.UINT 32),NONE),       
       ("w32ChkLshift", P.INLLSHIFT(P.UINT 32), NONE)       
      ]

end (* local *)

(* uList structure *)
val uList =
  let val ev = ST.special "uListVar"
      val allElements = 
            [(S.tycSymbol "list", M.TYCspec{spec=BT.ulistTycon,entVar=ev,
					    repl=false,scope=0}),
              mkConElement("nil", BT.unilDcon),
              mkConElement("::", BT.uconsDcon)]
      val allSymbols = map #1 allElements

   in M.STR{sign=M.SIG{name=NONE, closed=true, 
                       fctflag=false, stamp=ST.special "uListSig",
                       symbols=allSymbols, elements=allElements,
                       typsharing=nil, strsharing=nil,
                       boundeps=ref (SOME []), lambdaty=ref NONE}, 

            rlzn={stamp=ST.special "uListStr",
                  entities=EE.bind(ev,M.TYCent BT.ulistTycon,EE.empty),
                  lambdaty=ref(NONE),
                  rpath=IP.IPATH[S.strSymbol "uList"]},

            access=A.nullAcc, info=II.mkStrInfo[]}
  end

(* inLine structure *)
val inLine =
  let val bottom = T.POLYty{sign=[false], 
                            tyfun=T.TYFUN{arity=1,body=T.IBOUND 0}}
      (*
       * Using bottom here is a major gross hack. In the future, the actual
       * type should be given in the P.allPrimops list. Right now, only
       * polymorphic primOps are given the type --- to order to support
       * the type-based analysis correctly. (ZHONG)
       *)

      fun mkVarElement((name, p, tyOp),(symbols,elements,dacc,offset)) =
        let val s = S.varSymbol name
            val t = case tyOp of NONE => bottom
                               | SOME x => x
            val sp = M.VALspec{spec=t, slot=offset}
            val d = II.mkPrimInfo(p, tyOp)
         in (s::symbols, (s,sp)::elements, d::dacc, offset+1)
        end
      
      val (allSymbols, allElements, infList, _) = 
            foldl mkVarElement ([],[],[],0) allPrimops

      val (allSymbols, allElements, infList) = 
            (rev allSymbols, rev allElements, rev infList)

   in M.STR{sign=M.SIG{name=NONE, closed=true, 
                       fctflag=false, stamp=ST.special "inLineSig",
                       symbols=allSymbols, elements=allElements,
                       typsharing=nil, strsharing=nil,
                       boundeps=ref (SOME []), lambdaty=ref NONE},

            rlzn={stamp=ST.special "inLineStr", entities=EE.empty,
                  lambdaty=ref(NONE),
                  rpath=IP.IPATH[S.strSymbol "inLine"]},

	    access=A.nullAcc, info=(II.mkStrInfo infList)}
  end

(* priming structures: PrimTypes and InLine *)
val nameofPT = S.strSymbol "PrimTypes"
val nameofUL = S.strSymbol "UnrolledList"
val nameofIL = S.strSymbol "InLine"

val primEnv =
      SE.bind(nameofIL,B.STRbind inLine,
          SE.bind(nameofUL,B.STRbind uList,
  	     SE.bind(nameofPT,B.STRbind primTypes,
                MU.openStructure(SE.empty,primTypes))))


end (* local *)
end (* structure PrimEnv *)



(*
 * $Log: prim.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:36  george
 * Version 110.5
 *
 *)
