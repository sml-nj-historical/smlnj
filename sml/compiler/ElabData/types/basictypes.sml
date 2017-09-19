(* Copyright 1996 by AT&T Bell Laboratories *)
(* basictypes.sml *)

structure BasicTypes : BASICTYPES =
struct

local open Access Types Symbol
      structure EM = ErrorMsg
      structure IP = InvPath
      structure CBT = CoreBasicTypes
      structure PTN = PrimTycNum
  fun bug msg = ErrorMsg.impossible("BasicTypes: "^msg)
in

(*** type and dataconstructor symbols ***)
val unitSym      = (* tycSymbol "unit" *) CBT.unitSym
val boolSym      = tycSymbol "bool"
val trueSym	 = varSymbol "true"
val falseSym	 = varSymbol "false"
val listSym	 = tycSymbol "list"
val consSym	 = varSymbol "::"
val nilSym	 = varSymbol "nil"
val refConSym	 = (* varSymbol "ref" *) CBT.refConSym
val refTycSym	 = (* tycSymbol "ref" *) CBT.refTycSym
val fragSym      = tycSymbol "frag"
val antiquoteSym = varSymbol "ANTIQUOTE"
val quoteSym     = varSymbol "QUOTE"
val suspSym      = tycSymbol "susp"  (* LAZY *)
val dollarSym    = varSymbol "$"     (* LAZY *)

(*** primitive type constructors and types ***)

(*** function type constructor ***)
infix -->
val arrowStamp = (* Stamps.special "->" *) CBT.arrowStamp
val arrowTycon = CBT.arrowTycon
val op --> = CBT.-->
(*
val arrowTycon =
    GENtyc { stamp = arrowStamp, path = IP.IPATH [tycSymbol "->"],
	     arity = 2, eq = ref NO,
	     kind = PRIMITIVE PTN.ptn_arrow,
	     stub = NONE}
fun t1 --> t2 = CONty(arrowTycon,[t1,t2])
*)
fun isArrowType(CONty(GENtyc { stamp, ... }, _)) = Stamps.eq(stamp, arrowStamp)
  | isArrowType(VARty(ref(INSTANTIATED ty))) = isArrowType ty
  | isArrowType(MARKty(tyc, region)) = isArrowType tyc
  | isArrowType _ = false
fun domain(CONty(_,[ty,_])) = ty
  | domain(MARKty(ty, region)) = domain ty
  | domain _ = bug "domain"
fun range(CONty(_,[_,ty])) = ty
  | range(MARKty(ty, region)) = range ty
  | range _ = bug "range"


(*** primitive types ***)

fun mkpt (sym,arity,eqprop,ptn) =
    GENtyc{stamp = Stamps.special sym, path = IP.IPATH[tycSymbol sym],
	   arity = arity, eq = ref eqprop,
	   kind = PRIMITIVE ptn,
	   stub = NONE}

val word32Tycon = mkpt("word32", 0, YES, PTN.ptn_int32)
val word32Ty = CONty(word32Tycon, nil)

val w32pairTycon =
    DEFtyc { stamp = Stamps.special "w32pair",
	     tyfun = TYFUN { arity = 0,
			     body = CBT.tupleTy [word32Ty, word32Ty] },
	     path = IP.IPATH [tycSymbol "w32pair"],
	     strict = [] }

fun mk64 sym =
    GENtyc { stamp = Stamps.special sym, arity = 0, eq = ref YES,
	     path = IP.IPATH [tycSymbol sym], stub = NONE,
	     kind = ABSTRACT w32pairTycon }

val intTycon = (* mkpt ("int", 0, YES, PTN.ptn_int31) *) CBT.intTycon
val intTy = (* CONty(intTycon, nil) *) CBT.intTy

val int32Tycon = mkpt ("int32", 0, YES, PTN.ptn_int32)
val int32Ty = CONty(int32Tycon, nil)

val int64Tycon = mk64 "int64"
val int64Ty = CONty (int64Tycon, [])

val intinfTycon = mkpt ("intinf", 0, YES, PTN.ptn_intinf)
val intinfTy = CONty(intinfTycon, nil)

val realTycon = (* mkpt("real", 0, NO, PTN.ptn_real) *) CBT.realTycon
val realTy = (* CONty(realTycon, nil) *) CBT.realTy

val wordTycon = mkpt("word", 0, YES, PTN.ptn_int31)
val wordTy = CONty(wordTycon, nil)

val word8Tycon = mkpt("word8", 0, YES, PTN.ptn_int31)
val word8Ty = CONty(word8Tycon, nil)

val word64Tycon = mk64 "word64"
val word64Ty = CONty (word64Tycon, [])

val stringTycon = (* mkpt("string", 0, YES, PTN.ptn_string) *) CBT.stringTycon
val stringTy = (* CONty(stringTycon, nil) *) CBT.stringTy

val charTycon = (* mkpt("char", 0, YES, PTN.ptn_int31) *) CBT.charTycon
val charTy = (* CONty(charTycon, nil) *) CBT.charTy

val exnTycon = (* mkpt("exn", 0, NO, PTN.ptn_exn) *) CBT.exnTycon
val exnTy = (* CONty(exnTycon, nil) *) CBT.exnTy

val contTycon = mkpt("cont", 1, NO, PTN.ptn_cont)
val ccontTycon = mkpt("control_cont", 1, NO, PTN.ptn_ccont)

val arrayTycon = (* mkpt("array", 1, OBJ, PTN.ptn_array) *) CBT.arrayTycon

val vectorTycon = (* mkpt("vector", 1, YES, PTN.ptn_vector) *) CBT.vectorTycon

val objectTycon = mkpt("object", 0, NO, PTN.ptn_obj)

val c_functionTycon = mkpt("c_function", 0, NO, PTN.ptn_cfun)

val word8arrayTycon = mkpt("word8array", 0, OBJ, PTN.ptn_barray)

val real64arrayTycon = mkpt("real64array", 0, OBJ, PTN.ptn_rarray)

val spin_lockTycon = mkpt("spin_lock", 0, NO, PTN.ptn_slock)


(*** building record and product types ***)

val recordTy = CBT.recordTy
(*
fun recordTy(fields: (label * ty) list) : ty =
    CONty(Tuples.mkRECORDtyc(map (fn (a,b) => a) fields),
	  (map (fn(a,b)=>b) fields))
*)

val tupleTy = CBT.tupleTy
(*
fun tupleTy(tys: ty list) : ty =
    CONty(Tuples.mkTUPLEtyc(length tys), tys)
*)

fun getFields (CONty(RECORDtyc _, fl)) = SOME fl
  | getFields(MARKty(tyc, region)) = getFields tyc
  | getFields (VARty(ref(INSTANTIATED ty))) = getFields ty
  | getFields _ = NONE

(*
(*
 * I believe that unitTycon only needs to be a DEFtyc because of
 * the "structure PrimTypes = struct open PrimTypes end" declaration
 * in boot/built-in.sml.  This in turn is only necessary because of
 * a problem with the access assigned to PrimTypes. - DBM
 *)
val unitTycon = DEFtyc{stamp=Stamps.special "unit",
                       tyfun=TYFUN{arity=0,body=CONty(Tuples.mkTUPLEtyc 0,[])},
		       strict=[],path=IP.IPATH[unitSym]}
(* val unitTycon = Tuples.mkTUPLEtyc 0 *)
val unitTy = CONty(unitTycon, nil)
*)
val unitTycon = CBT.unitTycon
val unitTy = CBT.unitTy

(*** predefined datatypes ***)
val alpha = IBOUND 0

(* primitive datatypes *)

(* bool *)

val boolStamp = (* Stamps.special "bool" *) CBT.boolStamp
val boolsign = (* CSIG(0,2) *) CBT.boolsign
(*
val booleq = ref YES
val kind =
  DATATYPE{index=0, stamps= #[boolStamp], freetycs=[], root=NONE,
           family={members= #[{tycname=boolSym, eq=booleq, lazyp=false,
			       arity=0, sign=boolsign,
			       dcons=[{name=falseSym,rep=CONSTANT 0,
				       domain=NONE},
				      {name=trueSym,rep=CONSTANT 1,
				       domain=NONE}]}],
		   properties = PropList.newHolder (),
                   (* lambdatyc=ref NONE, *)
		   mkey=boolStamp}}


val boolTycon =
    GENtyc{stamp = boolStamp, path = IP.IPATH[boolSym],
	   arity = 0, eq = booleq, kind = kind, stub = NONE}
val boolTy = CONty(boolTycon,nil)
val falseDcon =
    DATACON
      {name = falseSym,
       const = true,
       lazyp = false,
       rep = CONSTANT 0,
       typ = boolTy,
       sign = boolsign}
val trueDcon =
    DATACON
      {name = trueSym,
       const = true,
       lazyp = false,
       rep = CONSTANT 1,
       typ = boolTy,
       sign = boolsign}
*)
val boolTycon = CBT.boolTycon
val boolTy = CBT.boolTy
val falseDcon = CBT.falseDcon
val trueDcon = CBT.trueDcon

(*
(* references *)

val refDom = alpha
val refStamp = Stamps.special "ref"
val refsign = CSIG(1,0)
val refEq = ref OBJ
val kind =
  DATATYPE{index=0, stamps= #[refStamp], freetycs=[], root=NONE,
           family={members= #[{tycname=refTycSym, eq=refEq, lazyp=false,
                               arity=1, sign=refsign,
			       dcons=[{name=refConSym,rep=REF,
				       domain=SOME refDom}]}],
		   properties = PropList.newHolder (),
                   (* lambdatyc=ref NONE, *)
		   mkey=refStamp}}

val refTycon =
    GENtyc{stamp = refStamp, path = IP.IPATH[refTycSym],
	   arity = 1, eq = refEq, kind = kind, stub = NONE}
val refTyfun = TYFUN{arity = 1, body = alpha --> CONty(refTycon, [alpha])}
val refDcon =
    DATACON
      {name = refConSym,
       const = false,
       lazyp = false,
       rep = REF,
       typ = POLYty{sign = [false], tyfun = refTyfun},
       sign = refsign}
val refPatType = POLYty {sign = [false], tyfun = refTyfun}
*)
val refTycon = CBT.refTycon
val refPatType = CBT.refPatType
val refDcon = CBT.refDcon


(* lists *)

val listStamp = Stamps.special "list"
val consDom = tupleTy[alpha, CONty(RECtyc 0,[alpha])]
val listsign = CSIG(1,1) (* [UNTAGGED,CONSTANT 0], [LISTCONS,LISTNIL] *)
val listeq = ref YES
val kind =
  DATATYPE{index=0, stamps= #[listStamp], freetycs=[], root=NONE, stripped=false,
           family={members=
                        #[{tycname=listSym, eq=listeq, lazyp=false,
			   arity=1, sign=listsign,
			   dcons=[{name=consSym,rep=UNTAGGED,
				   domain=SOME consDom},
			           {name=nilSym,rep=CONSTANT 0,domain=NONE}]}],
		   properties = PropList.newHolder (),
		   (* lambdatyc=ref NONE, *)
		   mkey=listStamp}}

val listTycon =
    GENtyc{stamp = listStamp, path = IP.IPATH[listSym], arity = 1,
	   eq = listeq, kind = kind, stub = NONE}
val consDcon =
    DATACON
      {name = consSym,
       const = false,
       lazyp = false,
       rep = UNTAGGED,   (* was LISTCONS *)
       typ = POLYty{sign = [false],
		    tyfun = TYFUN
			    {arity = 1,
			     body = CONty(arrowTycon,
				      [tupleTy[alpha, CONty(listTycon,[alpha])],
				       CONty(listTycon,[alpha])])}},
       sign = listsign}
val nilDcon =
    DATACON
      {name = nilSym,
       const = true,
       lazyp = false,
       rep = CONSTANT 0, (* was LISTNIL *)
       typ = POLYty {sign = [false],
		     tyfun = TYFUN{arity=1,body=CONty(listTycon,[alpha])}},
       sign = listsign}


(* unrolled lists *)
(* should this type have a different stamp from list? *)
val ulistStamp = Stamps.special "ulist"
val ulistsign = CSIG(1,1) (* [LISTCONS,LISTNIL] *)
val ulistEq = ref YES
val kind =
  DATATYPE{index=0, stamps= #[ulistStamp], freetycs=[], root=NONE, stripped=false,
	   family={members= #[{tycname=listSym, eq=ulistEq, lazyp=false,
			       arity=1, sign=ulistsign,
			dcons=[{name=consSym,rep=LISTCONS,
				domain=SOME consDom},
			     {name=nilSym,rep=LISTNIL,domain=NONE}]}],
		   properties = PropList.newHolder (),
                   (* lambdatyc=ref NONE, *)
		   mkey=ulistStamp}}

val ulistTycon =
    GENtyc{stamp = ulistStamp, path = IP.IPATH[listSym], arity = 1,
	   eq = ulistEq, kind = kind, stub = NONE}

val uconsDcon =
   DATACON
    {name = consSym,
     const = false,
     lazyp = false,
     rep = LISTCONS,
     typ = POLYty
            {sign = [false],
	     tyfun = TYFUN{arity = 1,
			   body = CONty(arrowTycon,
					[tupleTy[alpha,CONty(ulistTycon,[alpha])],
					 CONty(ulistTycon,[alpha])])}},
     sign = ulistsign}

val unilDcon =
   DATACON
    {name = nilSym,
     const = true,
     lazyp = false,
     rep = LISTNIL,
     typ = POLYty {sign = [false],
		   tyfun = TYFUN{arity=1,body=CONty(ulistTycon,[alpha])}},
     sign = ulistsign}


(* frags *)

val antiquoteDom = alpha
val quoteDom = stringTy
val fragStamp = Stamps.special "frag"
val fragsign = CSIG(2, 0) (* [TAGGED 0, TAGGED 1] *)
val frageq = ref YES
val kind =
  DATATYPE{index=0, stamps= #[fragStamp], freetycs=[], root=NONE, stripped=false,
           family={members= #[{tycname=fragSym, eq=frageq, lazyp=false,
				arity=1, sign=fragsign,
				dcons=[{name=antiquoteSym,rep=TAGGED 0,
					domain=SOME antiquoteDom},
				       {name=quoteSym,rep=TAGGED 1,
					domain=SOME quoteDom}]}],
		   properties = PropList.newHolder (),
                   (* lambdatyc=ref NONE, *)
		   mkey=fragStamp}}

(* predefine path as "SMLofNJ.frag", since it will be replicated into
 * the SMLofNJ structure *)
val fragTycon =
    GENtyc{stamp = fragStamp, path = IP.IPATH[fragSym,strSymbol "SMLofNJ"],
	   arity = 1, eq = frageq, kind = kind, stub = NONE}
val ANTIQUOTEDcon =
    DATACON
      {name = antiquoteSym,
       const = false,
       lazyp = false,
       rep = TAGGED 0,
       typ = POLYty {sign = [false],
		     tyfun = TYFUN
			    {arity = 1,
			     body = CONty(arrowTycon,
				      [alpha, CONty(fragTycon,[alpha])])}},
       sign = fragsign}
val QUOTEDcon =
    DATACON
      {name = quoteSym,
       const = false,
       lazyp = false,
       rep = TAGGED 1,
       typ = POLYty {sign = [false],
		     tyfun = TYFUN
			    {arity = 1,
			     body = CONty(arrowTycon,
				      [stringTy, CONty(fragTycon,[alpha])])}},
       sign = fragsign}

(* LAZY: suspensions for supporting lazy evaluation *)
val dollarDom = alpha
val suspStamp = Stamps.special "susp"
val suspsign = CSIG(1,0)
val suspEq = ref NO
val kind =
   DATATYPE{index=0, stamps= #[suspStamp], freetycs=[], root=NONE, stripped=false,
            family={members= #[{tycname=dollarSym,eq=suspEq, lazyp=false,
                                arity=1, sign=suspsign,
                                dcons=[{name=dollarSym,
                                        rep=SUSP(NONE),
                                        domain=SOME dollarDom}]}],
		    properties = PropList.newHolder (),
                    (* lambdatyc=ref NONE, *)
		    mkey=suspStamp}}

val suspTycon =
    GENtyc{stamp = suspStamp, path = IP.IPATH[suspSym],
           arity = 1, eq = suspEq, kind = kind, stub = NONE}
val suspTyfun =
    TYFUN{arity = 1, body = dollarDom --> CONty(suspTycon, [alpha])}
val dollarDcon =
    DATACON
      {name = dollarSym,
       const = false,
       lazyp = false,
       rep = SUSP(NONE),
       typ = POLYty{sign = [false], tyfun = suspTyfun},
       sign = suspsign}
val suspPatType = POLYty {sign = [false], tyfun = suspTyfun}

end (* local *)
end (* structure BasicTypes *)
