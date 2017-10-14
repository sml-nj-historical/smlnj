(* basictypes.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure BasicTypes : BASICTYPES =
  struct

    structure EM = ErrorMsg
    structure IP = InvPath
    structure T = Types
    structure IP = InvPath

    fun bug msg = ErrorMsg.impossible("BasicTypes: "^msg)

  (*** type and dataconstructor symbols ***)
    val unitSym		= Symbol.tycSymbol "unit"
    val boolSym		= Symbol.tycSymbol "bool"
    val trueSym		= Symbol.varSymbol "true"
    val falseSym	= Symbol.varSymbol "false"
    val listSym		= Symbol.tycSymbol "list"
    val consSym		= Symbol.varSymbol "::"
    val nilSym		= Symbol.varSymbol "nil"
    val refTycSym	= Symbol.tycSymbol "ref"
    val refConSym	= Symbol.varSymbol "ref"
    val fragSym		= Symbol.tycSymbol "frag"
    val antiquoteSym	= Symbol.varSymbol "ANTIQUOTE"
    val quoteSym	= Symbol.varSymbol "QUOTE"
    val suspSym		= Symbol.tycSymbol "susp"  (* LAZY *)
    val dollarSym	= Symbol.varSymbol "$"     (* LAZY *)

  (*** primitive type constructors and types ***)

  (*** function type constructor ***)
    val arrowStamp = Stamps.special "->"
    val arrowTycon = T.GENtyc {
	    stamp = arrowStamp,
	     path = IP.IPATH [Symbol.tycSymbol "->"],
	     arity = 2,
	     eq = ref T.NO,
	     kind = T.PRIMITIVE,
	     stub = NONE
	  }
    infix -->
    fun t1 --> t2 = T.CONty (arrowTycon, [t1, t2])

    fun isArrowType (T.CONty(T.GENtyc { stamp, ... }, _)) = Stamps.eq(stamp, arrowStamp)
      | isArrowType (T.VARty(ref(T.INSTANTIATED ty))) = isArrowType ty
      | isArrowType (T.MARKty(tyc, region)) = isArrowType tyc
      | isArrowType _ = false
    fun domain (T.CONty(_,[ty,_])) = ty
      | domain (T.MARKty(ty, region)) = domain ty
      | domain _ = bug "domain"
    fun range (T.CONty(_,[_,ty])) = ty
      | range (T.MARKty(ty, region)) = range ty
      | range _ = bug "range"

  (*** building record and product types ***)

    fun recordTy (fields: (T.label * T.ty) list) =
	  T.CONty (Tuples.mkRECORDtyc (map #1 fields), map #2 fields)

    fun tupleTy tys = T.CONty (Tuples.mkTUPLEtyc (length tys), tys)

    fun getFields (T.CONty(T.RECORDtyc _, fl)) = SOME fl
      | getFields (T.MARKty(tyc, region)) = getFields tyc
      | getFields (T.VARty(ref(T.INSTANTIATED ty))) = getFields ty
      | getFields _ = NONE

(*
 * I believe that unitTycon only needs to be a DEFtyc because of
 * the "structure PrimTypes = struct open PrimTypes end" declaration
 * in boot/built-in.sml.  This in turn is only necessary because of
 * a problem with the access assigned to PrimTypes. - DBM
 *)
    val unitTycon = T.DEFtyc {
	    stamp = Stamps.special "unit",
	    tyfun = T.TYFUN { arity = 0, body = T.CONty(Tuples.mkTUPLEtyc 0, []) },
	    strict = [],
	    path = IP.IPATH [unitSym]
	  }

    val unitTy = T.CONty(unitTycon, [])

  (*** primitive types ***)

    fun mkpt (sym, arity, eqprop) = T.GENtyc{
	    stamp = Stamps.special sym,
            path = IP.IPATH[Symbol.tycSymbol sym],
	    arity = arity,
            eq = ref eqprop,
	    kind = T.PRIMITIVE,
	    stub = NONE
	  }

    val word32Tycon = mkpt("word32", 0, T.YES)
    val word32Ty = T.CONty(word32Tycon, nil)

  (* used to represent Int64.int on 32-bit machines *)
    val w32pairTycon = T.DEFtyc {
	    stamp = Stamps.special "w32pair",
	    tyfun = T.TYFUN { arity = 0, body = tupleTy [word32Ty, word32Ty] },
	    path = IP.IPATH [Symbol.tycSymbol "w32pair"],
	    strict = []
	  }

    fun mk64 sym = T.GENtyc {
	    stamp = Stamps.special sym, arity = 0, eq = ref T.YES,
	    path = IP.IPATH [Symbol.tycSymbol sym], stub = NONE,
	    kind = T.ABSTRACT w32pairTycon
	  }

    fun pt2tct args = let
	  val tyc = mkpt args
          in
	    (tyc, T.CONty (tyc, []))
	  end

    val (intTycon, intTy) = pt2tct ("int", 0, T.YES)
    val (int32Tycon, int32Ty) = pt2tct ("int32", 0, T.YES)
    val (int64Tycon, int64Ty) = if Target.is64
	  then pt2tct ("int64", 0, T.YES)
	  else let
	  (* use pairs of ints to represent 64-bit ints on 32-bit machines *)
	    val int64Tycon = mk64 "int64"
	    in
	      (int64Tycon, T.CONty (int64Tycon, []))
	    end

    val (intinfTycon, intinfTy) = pt2tct ("intinf", 0, T.YES)
    val (realTycon, realTy) = pt2tct ("real", 0, T.NO)

    val arrayTycon = mkpt ("array", 1, T.OBJ)
    val vectorTycon = mkpt ("vector", 1, T.YES)

    val (wordTycon, wordTy) = pt2tct("word", 0, T.YES)
    val (word8Tycon, word8Ty) = pt2tct("word8", 0, T.YES)

    val word64Tycon = mk64 "word64"
    val word64Ty = T.CONty (word64Tycon, [])

    val (stringTycon, stringTy) = pt2tct ("string", 0, T.YES)

    val (charTycon, charTy) = pt2tct ("char", 0, T.YES)

    val (exnTycon, exnTy) = pt2tct ("exn", 0, T.NO)

    val contTycon = mkpt("cont", 1, T.NO)
    val ccontTycon = mkpt("control_cont", 1, T.NO)

    val arrayTycon = mkpt ("array", 1, T.OBJ)
    val vectorTycon = mkpt ("vector", 1, T.YES)

    val objectTycon = mkpt("object", 0, T.NO)

    val c_functionTycon = mkpt("c_function", 0, T.NO)

    val word8arrayTycon = mkpt("word8array", 0, T.OBJ)

    val real64arrayTycon = mkpt("real64array", 0, T.OBJ)

    val spin_lockTycon = mkpt("spin_lock", 0, T.NO)

  (*** predefined datatypes ***)
    val alpha = T.IBOUND 0

  (* primitive datatypes *)

  (* bool *)
    val boolStamp = Stamps.special "bool"
    val boolsign = Access.CSIG (0, 2)
    val (boolTycon, boolTy, falseDcon, trueDcon) = let
	  val booleq = ref T.YES
	  val boolTycon = T.GENtyc {
		  stamp = boolStamp,
		  path = IP.IPATH [boolSym],
		  arity = 0,
		  eq = booleq,
		  kind = T.DATATYPE{
		      index = 0,
		      stamps = #[boolStamp],
		      freetycs = [],
		      root = NONE,
		      stripped = false,
		      family = {
			  members = #[{ tycname = boolSym,
				     eq = booleq,
				     lazyp = false,
				     arity = 0,
				     sign = boolsign,
				     dcons = [{ name = falseSym,
						rep = Access.CONSTANT 0,
						domain = NONE },
					      { name = trueSym,
						rep = Access.CONSTANT 1,
						domain = NONE }]}],
			  properties = PropList.newHolder (),
			  mkey = boolStamp
			}
		    },
		  stub = NONE
		}
	  val boolTy = T.CONty (boolTycon, [])
	  val falseDcon = T.DATACON {
		  name = falseSym,
		  const = true,
		  lazyp = false,
		  rep = Access.CONSTANT 0,
		  typ = boolTy,
		  sign = boolsign
		}
	  val trueDcon = T.DATACON {
		  name = trueSym,
		  const = true,
		  lazyp = false,
		  rep = Access.CONSTANT 1,
		  typ = boolTy,
		  sign = boolsign
		}
	  in
	    (boolTycon, boolTy, falseDcon, trueDcon)
	  end

  (* references *)
    val refStamp = Stamps.special "ref"
    val (refTycon, refPatType, refDcon) = let
	val eqRef = ref T.OBJ
	val alpha = T.IBOUND 0
	val refDom = alpha
	val refsign = Access.CSIG (1, 0)
	val refTycon = T.GENtyc
		{ stamp = refStamp,
		  path = IP.IPATH [refTycSym],
		  arity = 1,
		  eq = eqRef,
		  kind = T.DATATYPE
		      { index = 0,
			stamps = #[refStamp],
			freetycs = [],
			root = NONE,
			stripped = false,
			family = { members =
				   #[{ tycname = refTycSym,
				       eq = eqRef,
				       lazyp = false,
				       arity = 1,
				       sign = Access.CSIG(1, 0),
				       dcons = [{ name = refConSym,
						  rep = Access.REF,
						  domain = SOME refDom }]}],
				   properties = PropList.newHolder (),
				   mkey = refStamp } },
		    stub = NONE }
	val refTyfun =
	    T.TYFUN { arity = 1, body = alpha --> T.CONty (refTycon, [alpha]) }
	val refPatType = T.POLYty { sign = [false], tyfun = refTyfun }
	val refDcon = T.DATACON { name = refConSym,
				  const = false,
				  lazyp = false,
				  rep = Access.REF,
				  typ = refPatType,
				  sign = refsign }
	in
	  (refTycon, refPatType, refDcon)
	end

  (* lists *)

    val listStamp = Stamps.special "list"
    val consDom = tupleTy[alpha, T.CONty(T.RECtyc 0,[alpha])]
    val listsign = Access.CSIG(1,1) (* [Access.UNTAGGED,Access.CONSTANT 0], [Access.LISTCONS,Access.LISTNIL] *)
    val listeq = ref T.YES
    val kind =
      T.DATATYPE{index=0, stamps= #[listStamp], freetycs=[], root=NONE, stripped=false,
	       family={members=
			    #[{tycname=listSym, eq=listeq, lazyp=false,
			       arity=1, sign=listsign,
			       dcons=[{name=consSym,rep=Access.UNTAGGED,
				       domain=SOME consDom},
				       {name=nilSym,rep=Access.CONSTANT 0,domain=NONE}]}],
		       properties = PropList.newHolder (),
		       (* lambdatyc=ref NONE, *)
		       mkey=listStamp}}

    val listTycon =
	T.GENtyc{stamp = listStamp, path = IP.IPATH[listSym], arity = 1,
	       eq = listeq, kind = kind, stub = NONE}
    val consDcon = T.DATACON{
	   name = consSym,
	   const = false,
	   lazyp = false,
	   rep = Access.UNTAGGED,   (* was Access.LISTCONS *)
	   typ = T.POLYty{sign = [false],
			tyfun = T.TYFUN
				{arity = 1,
				 body = T.CONty(arrowTycon,
					  [tupleTy[alpha, T.CONty(listTycon,[alpha])],
					   T.CONty(listTycon,[alpha])])}},
	   sign = listsign}
    val nilDcon = T.DATACON{
	   name = nilSym,
	   const = true,
	   lazyp = false,
	   rep = Access.CONSTANT 0, (* was Access.LISTNIL *)
	   typ = T.POLYty {sign = [false],
			 tyfun = T.TYFUN{arity=1,body=T.CONty(listTycon,[alpha])}},
	   sign = listsign}


(* unrolled lists *)
(* should this type have a different stamp from list? *)
    val ulistStamp = Stamps.special "ulist"
    val ulistsign = Access.CSIG(1,1) (* [Access.LISTCONS,Access.LISTNIL] *)
    val ulistEq = ref T.YES
    val kind = T.DATATYPE{
	    index=0, stamps= #[ulistStamp], freetycs=[], root=NONE, stripped=false,
	    family={
		members= #[{tycname=listSym, eq=ulistEq, lazyp=false,
				   arity=1, sign=ulistsign,
			    dcons=[{name=consSym,rep=Access.LISTCONS,
				    domain=SOME consDom},
				 {name=nilSym,rep=Access.LISTNIL,domain=NONE}]}],
		properties = PropList.newHolder (),
		mkey=ulistStamp}}

    val ulistTycon =
	T.GENtyc{stamp = ulistStamp, path = IP.IPATH[listSym], arity = 1,
	       eq = ulistEq, kind = kind, stub = NONE}

    val uconsDcon = T.DATACON{
	 name = consSym,
	 const = false,
	 lazyp = false,
	 rep = Access.LISTCONS,
	 typ = T.POLYty
		{sign = [false],
		 tyfun = T.TYFUN{arity = 1,
			       body = T.CONty(arrowTycon,
					    [tupleTy[alpha,T.CONty(ulistTycon,[alpha])],
					     T.CONty(ulistTycon,[alpha])])}},
	 sign = ulistsign}

    val unilDcon = T.DATACON{
	 name = nilSym,
	 const = true,
	 lazyp = false,
	 rep = Access.LISTNIL,
	 typ = T.POLYty {sign = [false],
		       tyfun = T.TYFUN{arity=1,body=T.CONty(ulistTycon,[alpha])}},
	 sign = ulistsign}

  (* frags *)

    val antiquoteDom = alpha
    val quoteDom = stringTy
    val fragStamp = Stamps.special "frag"
    val fragsign = Access.CSIG(2, 0) (* [Access.TAGGED 0, Access.TAGGED 1] *)
    val frageq = ref T.YES
    val kind = T.DATATYPE{
	   index=0, stamps= #[fragStamp], freetycs=[], root=NONE, stripped=false,
           family={members= #[{tycname=fragSym, eq=frageq, lazyp=false,
				arity=1, sign=fragsign,
				dcons=[{name=antiquoteSym,rep=Access.TAGGED 0,
					domain=SOME antiquoteDom},
				       {name=quoteSym,rep=Access.TAGGED 1,
					domain=SOME quoteDom}]}],
		   properties = PropList.newHolder (),
                   (* lambdatyc=ref NONE, *)
		   mkey=fragStamp}}

  (* predefine path as "SMLofNJ.frag", since it will be replicated into
   * the SMLofNJ structure *)
    val fragTycon = T.GENtyc{
	    stamp = fragStamp, path = IP.IPATH[fragSym, Symbol.strSymbol "SMLofNJ"],
	    arity = 1, eq = frageq, kind = kind, stub = NONE
	  }
    val ANTIQUOTEDcon = T.DATACON{
	    name = antiquoteSym,
	    const = false,
	    lazyp = false,
	    rep = Access.TAGGED 0,
	    typ = T.POLYty {sign = [false],
			  tyfun = T.TYFUN
				 {arity = 1,
				  body = T.CONty(arrowTycon,
					   [alpha, T.CONty(fragTycon,[alpha])])}},
	    sign = fragsign}
    val QUOTEDcon = T.DATACON{
	    name = quoteSym,
	    const = false,
	    lazyp = false,
	    rep = Access.TAGGED 1,
	    typ = T.POLYty {sign = [false],
			 tyfun = T.TYFUN
				{arity = 1,
				 body = T.CONty(arrowTycon,
					  [stringTy, T.CONty(fragTycon,[alpha])])}},
	   sign = fragsign}

  (* LAZY: suspensions for supporting lazy evaluation *)
    val dollarDom = alpha
    val suspStamp = Stamps.special "susp"
    val suspsign = Access.CSIG(1,0)
    val suspEq = ref T.NO
    val kind = T.DATATYPE{
	    index=0, stamps= #[suspStamp], freetycs=[], root=NONE, stripped=false,
	    family={members= #[{tycname=dollarSym,eq=suspEq, lazyp=false,
				    arity=1, sign=suspsign,
				    dcons=[{name=dollarSym,
					    rep=Access.SUSP NONE,
					    domain=SOME dollarDom}]}],
			properties = PropList.newHolder (),
			(* lambdatyc=ref NONE, *)
			mkey=suspStamp}}

    val suspTycon = T.GENtyc{
	    stamp = suspStamp, path = IP.IPATH[suspSym],
	    arity = 1, eq = suspEq, kind = kind, stub = NONE
	  }
    val suspTyfun = T.TYFUN{arity = 1, body = dollarDom --> T.CONty(suspTycon, [alpha])}
    val dollarDcon = T.DATACON{
	    name = dollarSym,
	    const = false,
	    lazyp = false,
	    rep = Access.SUSP(NONE),
	    typ = T.POLYty{sign = [false], tyfun = suspTyfun},
	    sign = suspsign
	  }
    val suspPatType = T.POLYty{sign = [false], tyfun = suspTyfun}

  end (* structure BasicTypes *)
