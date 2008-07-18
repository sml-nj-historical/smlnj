(* stringtoty.sml *)

signature STRINGTOTY = 
sig

    val stringToVar : string -> Ens_types2.var_elem list
    val stringToType : string -> Ens_types2.type_elem list
    val stringToCons : string -> Ens_types2.cons_elem list 
    val stringToStr : string -> Ens_types2.str_elem list 
    val stringToSig : string -> Ens_types2.sig_elem list
    val stringToExt : string -> Ens_types2.ext_elem list

    val stringToLvarExt : string -> (Access.access * Access.access) list
end (* signature STRINGTOTY *)

local
    open Ens_types2
in
structure StringToTy : STRINGTOTY =
struct

fun bug x = ErrorMsg.impossible ("StringToTy: " ^ x)

val tokenize : string -> string list = String.tokens Char.isSpace

val concat = String.concatWith " "

fun get_int s =
    case Int.fromString s of
	NONE => bug ("get_int: " ^ s)
      | SOME i => i

fun stringToList stringToA (h :: sl) = 
    let 
	val n = get_int h
	fun f sl0 0 = ([], sl0)
	  | f sl0 n = 
	    let val (ty, sl1) = stringToA sl0
		val (tyl, sl2) = f sl1 (n-1) in
		(ty :: tyl, sl2)
	    end
    in
	f sl n
    end	
  | stringToList _ [] = 
    bug "stringToList"

fun ext_fun f msg s = 
    case stringToList f (tokenize s) of
	(list, []) => list
      | (_, rem) => (print (concat rem); print "\n"; bug msg)

fun get_bool "t" = true
  | get_bool "f" = false
  | get_bool _ = bug "get_bool"

fun get_symbol s = 
    case String.tokens (fn c => c = #"$") s of
	["VAL",  name] => Symbol.varSymbol  name
      | ["SIG",  name] => Symbol.sigSymbol  name
      | ["STR",  name] => Symbol.strSymbol  name
      | ["FSIG", name] => Symbol.fsigSymbol name
      | ["FCT",  name] => Symbol.fctSymbol  name
      | ["TYC",  name] => Symbol.tycSymbol  name
      | ["LAB",  name] => Symbol.labSymbol  name
      | ["TYV",  name] => Symbol.tyvSymbol  name
      | ["FIX",  name] => Symbol.fixSymbol  name
      | _ => bug ("get_symbol: " ^ s)

fun get_persstamps s = 
    case PersStamps.fromHex s of
	NONE => bug ("get_persstamps" ^ s)
      | SOME st => st

fun stringToStamp ("SS" :: h :: sl) =
    (Stamps.special h, sl)
  | stringToStamp ("GS" :: h1 :: h2 :: sl) = 
    ( Stamps.global 
	  {pid = get_persstamps h1, cnt = get_int h2}, 
      sl
    )
  | stringToStamp ("FS" :: h :: sl) = 
	(Stamps.fresh' (get_int h), sl)
  | stringToStamp _ = 
    bug "stringToStamp"

fun stringToLabels (h :: sl) = 
    let val sl2 = String.tokens 
		      (fn c => c = #"." orelse c = #"<" orelse c = #">") 
		      h
	fun f (a::r) = Symbol.labSymbol a :: f r
	  | f nil = []
	val symbol_list = f sl2
    in
	(symbol_list, sl)
    end
  | stringToLabels [] = bug "stringToLabels"

fun stringToPath (h :: sl) = 
    let val sl2 = String.tokens (fn c => c = #".") h
    in
	case sl2 of
	    [] => bug "stringToPath.1"
	  | h' :: q' => ( InvPath.IPATH 
			      ( get_symbol h' :: 
				List.map Symbol.strSymbol q'),
			  sl
			)
    end
  | stringToPath [] = bug "stringToPath.2"

fun stringToTyc ("G" :: sl) = 
    let
	val (stamp, sl') = stringToStamp sl
	val (path, sl'') = stringToPath sl'
    in
	(General (stamp, path), sl'')
    end
  | stringToTyc ("R" :: sl) = 
    let val (l, sl') = stringToLabels sl in
	(Record l, sl')
    end
  | stringToTyc ("P" :: sl) = 
    let val (p, sl') = stringToPath sl in
	(Path p, sl')
    end
  | stringToTyc _ = bug "stringToTyc"

fun stringsToTy ("C" :: sl') =
    let
	val (stub, sl'') = stringToTyc sl'
	val (args, sl''') = stringToTyList sl''
    in
	(Conty (stub, args), sl''')
    end
  | stringsToTy ("I" :: h :: sl) = 
    (Ibound (get_int h), sl)
  | stringsToTy ("L" :: h1 :: h2 :: sl') =
    (Lbound {index = get_int h1, depth = get_int h2}, sl')
  | stringsToTy ("U" :: h :: sl') =
    let val s = Symbol.tycSymbol h in
	(Ubound s, sl')
    end
  | stringsToTy ("P" :: h :: sl') =
    let
	val arity = get_int h
	val (body, sl'') = stringsToTy sl'
    in
	(Poly {arity = arity, body = body}, sl'')
    end
  | stringsToTy _ = bug "stringsToTy"

and stringToTyList sl = 
    stringToList stringsToTy sl

fun stringToTy s = 
    #1 (stringsToTy (tokenize s))

fun stringsToTycon ("D" :: h1 :: sl) = 
    let val (labels, sl') = stringToLabels sl in
	(Datatype (get_bool h1, labels), sl')
    end
  | stringsToTycon ("A" :: sl) = 
    let val (labels, sl') = stringToLabels sl in
	(Abstract labels, sl')
    end
  | stringsToTycon ("F" :: sl) =
    (Deftyc, sl)
  | stringsToTycon ("P" :: h :: sl) = 
    (Primtyc (get_bool h), sl)
  | stringsToTycon _ = bug "stringsToTycon"

fun stringToTycon s = 
    #1 (stringsToTycon (tokenize s))

fun stringToString s = 
    case String.tokens (fn c => c = #"<" orelse c = #">") s of
	[] => ""
      | [s'] => s'
      | _ => bug "stringToString"

fun stringToLocation (h1 :: h2 :: h3 :: sl) =
    ((stringToString h1, get_int h2, get_int h3), sl)
  | stringToLocation _ = bug "stringToLocation"

fun stringToAccess ("L" :: h :: sl) = 
    (Access.LVAR (get_int h), sl)
  | stringToAccess ("E" :: h :: sl) = 
    (Access.EXTERN (get_persstamps h), sl)
  | stringToAccess ("P" :: h :: sl) = 
    let val (acc, sl') = stringToAccess sl in
	(Access.PATH (acc, get_int h), sl')
    end
  | stringToAccess _ = 
    bug "stringToAccess"

fun stringToVarElemUsage sl = 
    let
	fun f sl = 
	    let val (loc, sl0) = stringToLocation sl
		val (ty, sl1) = stringsToTy sl0
		val (acc, sl2) = stringToAccess sl1
	    in
		((loc, ty, acc), sl2)
	    end
    in
	stringToList f sl
    end

fun stringToVarElem sl = 
    let
	val (access, sl0) = stringToAccess sl
	val (name, sl1) = 
	    case sl0 of
		[] => bug "stringToVarElem.1"
	      | h :: sl1 => (Symbol.varSymbol h, sl1)
	val (parent, sl2) = stringToAccess sl1
	val (typ, sl3) = stringsToTy sl2
	val (def, sl4) = stringToLocation sl3
	val (usage, sl5) = stringToVarElemUsage sl4
    in
	({ access = access,
	  name = name,
	  parent = parent,
	  typ = typ,
	  def = def,
	  usage = ref usage
	}, sl5)
    end

val stringToVar = 
    ext_fun stringToVarElem "stringToVar"

fun stringToKey ("V" :: sl) = 
    let val (acc, sl') = stringToAccess sl in
	(Var acc, sl')
    end
  | stringToKey ("S" :: sl) = 
    let val (acc, sl') = stringToAccess sl in
	(Str acc, sl')
    end
  | stringToKey ("T" :: sl) =
    let val (st, sl') = stringToStamp sl in
	(Type st, sl')
    end
  | stringToKey ("C" :: sl) = 
    let val (st, sl') = stringToStamp sl in
	( case sl' of 
	      [] => bug "stringToKey.1"
	    | h :: sl'' => (Cons (st, Symbol.varSymbol h), sl'')
	)
    end
  | stringToKey ("G" :: sl) = 
    let val (st, sl') = stringToStamp sl in
	(Sig st, sl')
    end
  | stringToKey _ = 
    bug "stringToKey.2"

fun stringToElements ("D" :: sl) = 
    let fun f (x :: y :: sl0) = 
	    let val (k, sl1) = stringToKey sl0 in
		((get_int x, get_symbol y, k), sl1)
	    end
	  | f _ = bug "stringToElements.1"
	val (d, sl') = stringToList f sl
    in
	(Def d, sl')
    end
  | stringToElements ("C" :: sl) = 
    let fun f (x :: y :: z :: sl) = 
	    ((get_int x, get_symbol y, get_int z), sl)
	  | f _ = bug "StringToElements.2"
	val (l, sl') = stringToList f sl
	val (acc, sl'') = stringToAccess sl'
    in
	(Constraint (l, acc), sl'')
    end
  | stringToElements ("A" :: sl) = 
    let val (acc, sl') = stringToAccess sl in
	(Alias acc, sl')
    end
  | stringToElements _ = bug "stringToElements.3"

fun stringToOption _ ("N" :: sl) = (NONE, sl)
  | stringToOption stringToA ("S" :: sl) = 
    let val (v, sl') = stringToA sl in
	(SOME v, sl')
    end
  | stringToOption _ _ = bug "stringToOption"

fun stringToStrElemUsage sl = 
    stringToList stringToLocation sl

fun stringToStrElem (h :: sl0) = 
    let
	val name = Symbol.strSymbol h
	val (access, sl1) = stringToAccess sl0
	val (parent, sl2) = stringToOption stringToAccess sl1
	val (sign, sl3) = stringToOption stringToStamp sl2
	val (def, sl4) = stringToLocation sl3
	val (elements, sl5) = stringToElements sl4
	val (usage, sl6) = stringToStrElemUsage sl5
    in
	({ name = name,
	   access = access,
	   parent = parent,
	   sign = sign,
	   def = def,
	   elements = elements,
	   usage = ref usage
	 }, sl6)
    end
  | stringToStrElem [] = bug "stringToStrElem"

val stringToStr = 
    ext_fun stringToStrElem "stringToStr"

fun stringToSpecSig ("T" :: sl) = 
    let val (tycon, sl') = stringsToTycon sl in
	(Typ tycon, sl')
    end
  | stringToSpecSig ("V" :: sl) = 
    let val (ty, sl') = stringsToTy sl in
	(Val ty, sl')
    end
  | stringToSpecSig ("E" :: sl) =
    let val (ty, sl') = stringsToTy sl in
	(Exception ty, sl')
    end
  | stringToSpecSig ("N" :: h :: sl) = 
    let val symbol = Symbol.sigSymbol h
	val (stamp, sl') = stringToStamp sl 
    in
	(NamedStr (symbol, stamp), sl')
    end
  | stringToSpecSig ("I" :: sl) = 
    let val (el, sl') = stringToElementsSig sl in
	(InlineStr el, sl')
    end
  | stringToSpecSig _ = bug "stringToSpecSig"

and stringToElementsSig sl = 
    let fun f (h :: sl) = 
	    let val symbol = get_symbol h
		val (spec_sig, sl') = stringToSpecSig sl
	    in
		((symbol, spec_sig), sl')
	    end
	  | f [] = bug "stringToElementsSig"
    in
	stringToList f sl
    end

fun stringToSigElemUsage sl = 
    let
	fun f sl = 
	    let val (loc, sl') = stringToLocation sl
		val (symbol, sl'') = 
		    case sl' of
			[] => bug "stringToSigElemUsage"
		      | h :: sl'' => (Symbol.sigSymbol h, sl'')
	    in
		((loc, symbol), sl'')
	    end
    in
	stringToList f sl
    end

fun stringToSigElemAlias sl = 
    stringToSigElemUsage sl

fun stringToSigElem (h :: sl) = 
    let val name = Symbol.sigSymbol h
	val (stamp, sl0) = stringToStamp sl
	val (inferred, sl1) = 
	    case sl0 of
		[] => bug "stringToSigElem.1"
	      | h :: sl1 => (get_bool h, sl1)
	val (def, sl2) = stringToLocation sl1
	val (elements, sl3) = stringToElementsSig sl2
	val (alias, sl4) = stringToSigElemAlias sl3
	val (usage, sl5) = stringToSigElemUsage sl4
    in
	({ name = name,
	  stamp = stamp,
	  inferred = inferred,
	  def = def,
	  elements = elements,
	  alias = ref alias,
	  usage = ref usage
	}, sl5)
    end
  | stringToSigElem [] = bug "stringToSigElem.2"

val stringToSig = 
    ext_fun stringToSigElem "stringToSig"

fun stringToTypeElemUsage sl = 
    stringToList stringToLocation sl

fun stringToTypeElem sl = 
    let val (tycon, sl0) = stringsToTycon sl
	val (stamp, sl1) = stringToStamp sl0
	val (name, sl2) = 
	    case sl1 of
		[] => bug "stringToTypeElem.1"
	      | h :: sl2 => (Symbol.tycSymbol h, sl2)
	val (def, sl3) = stringToLocation sl2
	val (usage, sl4) = stringToTypeElemUsage sl3
    in
	( { tycon = tycon, 
	    stamp = stamp, 
	    name = name, 
	    def = def, 
	    usage = ref usage
	  }
	,sl4)
    end

val stringToType = 
    ext_fun stringToTypeElem "stringToType"

fun stringToConsElemUsage sl = 
    let fun f sl = 
	    let val (loc, sl') = stringToLocation sl
		val (ty, sl'') = stringsToTy sl'
	    in
		((loc, ty), sl'')
	    end
    in
	stringToList f sl
    end

fun stringToConsElem (h :: sl) = 
    let val name = Symbol.varSymbol h
	val (dataty, sl0) = stringToStamp sl
	val (def, sl1) = stringToLocation sl0
	val (ty, sl2) = stringsToTy sl1
	val (usage, sl3) = stringToConsElemUsage sl2
    in
	( { name = name,
	    dataty = dataty,
	    def = def,
	    ty = ty,
	    usage = ref usage
	  }, sl3)
    end
  | stringToConsElem [] = 
    bug "stringToConsElem"

val stringToCons = 
    ext_fun stringToConsElem "stringToCons"

fun stringToExtElem ("v" :: sl) = 
    let	val (access, sl0) = stringToAccess sl
	val (usage, sl1) = stringToVarElemUsage sl0
    in
	(ExtVar{access = access, usage = ref usage}, sl1)
    end
  | stringToExtElem ("s" :: sl) = 
    let	val (access, sl0) = stringToAccess sl
	val (usage, sl1) = stringToStrElemUsage sl0
    in
	(ExtStr{access = access, usage = ref usage}, sl1)
    end
  | stringToExtElem ("t" :: sl) =
    let	val (stamp, sl0) = stringToStamp sl
	val (usage, sl1) = stringToTypeElemUsage sl0
    in
	(ExtType{stamp = stamp, usage = ref usage}, sl1)
    end
  | stringToExtElem ("c" :: h ::sl) = 
    let val name = Symbol.varSymbol h
	val (stamp, sl0) = stringToStamp sl
	val (usage, sl1) = stringToConsElemUsage sl0
    in
	(ExtCons{name = name, stamp = stamp, usage = ref usage}, sl1)
    end
  | stringToExtElem ("g" :: sl) = 
    let val (stamp, sl0) = stringToStamp sl
	val (usage, sl1) = stringToSigElemUsage sl0
    in
	(ExtSig{stamp = stamp, usage = ref usage}, sl1)
    end
  | stringToExtElem _ =
    bug "stringToExtElem"

val stringToExt = 
    ext_fun stringToExtElem "stringToExt"

fun stringToLvarExtElem sl =
    let val (acc1, sl0) = stringToAccess sl
	val (acc2, sl1) = stringToAccess sl0
    in
	((acc1, acc2), sl1)
    end

val stringToLvarExt = 
    ext_fun stringToLvarExtElem "stringToLvarExt"

end (* structure StringToTy*)
end (* local *)
