(* core-basictypes.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * a generic part of basictypes.sml (not SML/NJ specific)
 *)
structure CoreBasicTypes : sig

    val arrowStamp : Stamps.stamp
    val arrowTycon : Types.tycon
    val --> : Types.ty * Types.ty -> Types.ty

    val refStamp : Stamps.stamp
    val refTycSym : Symbol.symbol
    val refConSym : Symbol.symbol
    val refTycon : Types.tycon
    val refDcon : Types.datacon
    val refPatType : Types.ty

    val boolStamp : Stamps.stamp
    val boolSym : Symbol.symbol
    val falseSym : Symbol.symbol
    val trueSym : Symbol.symbol
    val boolTycon : Types.tycon
    val boolTy : Types.ty
    val boolsign : Access.consig
    val falseDcon : Types.datacon
    val trueDcon : Types.datacon

    val unitSym : Symbol.symbol
    val unitTycon : Types.tycon
    val unitTy : Types.ty

    val intTycon : Types.tycon
    val intTy : Types.ty

    val stringTycon : Types.tycon
    val stringTy : Types.ty

    val charTycon : Types.tycon
    val charTy : Types.ty

    val realTycon : Types.tycon
    val realTy : Types.ty

    val exnTycon : Types.tycon
    val exnTy : Types.ty

    val tupleTy : Types.ty list -> Types.ty

    val recordTy : (Types.label * Types.ty) list -> Types.ty

    val arrayTycon : Types.tycon
    val vectorTycon : Types.tycon

end = struct

    structure T = Types
    structure IP = InvPath
    structure PTN = CorePrimTycNum

    val arrowStamp = Stamps.special "->"
    val refStamp = Stamps.special "ref"
    val boolStamp = Stamps.special "bool"

    val unitSym = Symbol.tycSymbol "unit"
    val refTycSym = Symbol.tycSymbol "ref"
    val refConSym = Symbol.varSymbol "ref"
    val boolSym = Symbol.tycSymbol "bool"
    val falseSym = Symbol.varSymbol "false"
    val trueSym = Symbol.varSymbol "true"

    fun tc2t tyc = T.CONty (tyc, [])

    val unitTycon =
	T.DEFtyc { stamp = Stamps.special "unit",
		   tyfun = T.TYFUN { arity = 0,
				     body = T.CONty
						(Tuples.mkTUPLEtyc 0, []) },
		   strict = [],
		   path = IP.IPATH [unitSym] }

    val unitTy = tc2t unitTycon

    fun pt2tc (sym, arity, eqprop, ptn) =
	T.GENtyc { stamp = Stamps.special sym,
		   path = IP.IPATH [Symbol.tycSymbol sym],
		   arity = arity,
		   eq = ref eqprop,
		   kind = T.PRIMITIVE ptn,
		   stub = NONE }

    fun pt2tct args = let
	val tyc = pt2tc args
    in
	(tyc, tc2t tyc)
    end

    val (intTycon, intTy) = pt2tct ("int", 0, T.YES, PTN.ptn_int)
    val (stringTycon, stringTy) = pt2tct ("string", 0, T.YES, PTN.ptn_string)
    val (charTycon, charTy) = pt2tct ("char", 0, T.YES, PTN.ptn_int)
    val (realTycon, realTy) = pt2tct ("real", 0, T.NO, PTN.ptn_real)
    val (exnTycon, exnTy) = pt2tct ("exn", 0, T.NO, PTN.ptn_exn)

    val arrayTycon = pt2tc ("array", 1, T.OBJ, PTN.ptn_array)
    val vectorTycon = pt2tc ("vector", 1, T.YES, PTN.ptn_vector)

    val arrowTycon =
	T.GENtyc { stamp = arrowStamp,
		   path = IP.IPATH [Symbol.tycSymbol "->"],
		   arity = 2,
		   eq = ref T.NO,
		   kind = T.PRIMITIVE PTN.ptn_arrow,
		   stub = NONE }

    infix -->
    fun t1 --> t2 = T.CONty (arrowTycon, [t1, t2])

    fun recordTy (fields: (T.label * T.ty) list) =
	T.CONty (Tuples.mkRECORDtyc (map #1 fields), map #2 fields)

    fun tupleTy tys = T.CONty (Tuples.mkTUPLEtyc (length tys), tys)

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

    val boolsign = Access.CSIG (0, 2)
    val (boolTycon, boolTy, falseDcon, trueDcon) = let
	val booleq = ref T.YES
	val boolTycon =
	    T.GENtyc { stamp = boolStamp,
		       path = IP.IPATH [boolSym],
		       arity = 0,
		       eq = booleq,
		       kind = T.DATATYPE
		         { index = 0,
			   stamps = #[boolStamp],
			   freetycs = [],
			   root = NONE,
			   family =
			     { members =
			         #[{ tycname = boolSym,
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
			       mkey = boolStamp }},
		       stub = NONE }
	val boolTy = T.CONty (boolTycon, [])
	val falseDcon = T.DATACON { name = falseSym,
				    const = true,
				    lazyp = false,
				    rep = Access.CONSTANT 0,
				    typ = boolTy,
				    sign = boolsign }
	val trueDcon = T.DATACON { name = trueSym,
				    const = true,
				    lazyp = false,
				    rep = Access.CONSTANT 1,
				    typ = boolTy,
				    sign = boolsign }
    in
	(boolTycon, boolTy, falseDcon, trueDcon)
    end
end
