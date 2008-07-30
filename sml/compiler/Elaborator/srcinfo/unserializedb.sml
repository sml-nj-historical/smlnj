(* unserializedb.sml *)

local
    open DBTypes
in
structure UnSerializeDB : UNSERIALIZE_DB =
struct

fun bug x = ErrorMsg.impossible ("StringToTy: " ^ x)

val tokenize : string -> string list = String.tokens Char.isSpace

val concat = String.concatWith " "

fun get_int s =
    case Int.fromString s of
	NONE => bug ("get_int: " ^ s)
      | SOME i => i

fun stringsToList stringsToA (h :: sl) = 
    let 
	val n = get_int h
	fun f sl0 0 = ([], sl0)
	  | f sl0 n = 
	    let val (ty, sl1) = stringsToA sl0
		val (tyl, sl2) = f sl1 (n-1) in
		(ty :: tyl, sl2)
	    end
    in
	f sl n
    end	
  | stringsToList _ [] = 
    bug "stringsToList"

fun ext_fun f msg s = 
    case stringsToList f (tokenize s) of
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

fun stringsToPersstamps (h :: sl) = 
    (get_persstamps h, sl)
  | stringsToPersstamps [] = 
    bug "stringsToPersstamps"

fun stringsToStamp ("SS" :: h :: sl) =
    (Stamps.special h, sl)
  | stringsToStamp ("GS" :: h1 :: h2 :: sl) = 
    ( Stamps.global 
	  {pid = get_persstamps h1, cnt = get_int h2}, 
      sl
    )
  | stringsToStamp ("FS" :: h :: sl) = 
	(Stamps.fresh' (get_int h), sl)
  | stringsToStamp _ = 
    bug "stringsToStamp"

fun stringsToLabels (h :: sl) = 
    let val sl2 = String.tokens 
		      (fn c => c = #"." orelse c = #"<" orelse c = #">") 
		      h
	fun f (a::r) = Symbol.labSymbol a :: f r
	  | f nil = []
	val symbol_list = f sl2
    in
	(symbol_list, sl)
    end
  | stringsToLabels [] = bug "stringsToLabels"

fun stringsToPath (h :: sl) = 
    let val sl2 = String.tokens (fn c => c = #".") h
    in
	case sl2 of
	    [] => bug "stringsToPath.1"
	  | h' :: q' => ( InvPath.IPATH 
			      ( get_symbol h' :: 
				List.map Symbol.strSymbol q'),
			  sl
			)
    end
  | stringsToPath [] = bug "stringsToPath.2"

fun stringsToTyc ("G" :: sl) = 
    let
	val (stamp, sl') = stringsToStamp sl
	val (path, sl'') = stringsToPath sl'
    in
	(General (stamp, path), sl'')
    end
  | stringsToTyc ("R" :: sl) = 
    let val (l, sl') = stringsToLabels sl in
	(Record l, sl')
    end
  | stringsToTyc ("P" :: sl) = 
    let val (p, sl') = stringsToPath sl in
	(Path p, sl')
    end
  | stringsToTyc _ = bug "stringsToTyc"

fun stringsToTy ("C" :: sl') =
    let
	val (stub, sl'') = stringsToTyc sl'
	val (args, sl''') = stringsToTyList sl''
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

and stringsToTyList sl = 
    stringsToList stringsToTy sl

(*fun stringToTy s = 
    #1 (stringsToTy (tokenize s))*)

fun stringsToTycon ("D" :: h1 :: sl) = 
    let val (labels, sl') = stringsToLabels sl in
	(Datatype (get_bool h1, labels), sl')
    end
  | stringsToTycon ("A" :: sl) = 
    let val (labels, sl') = stringsToLabels sl in
	(Abstract labels, sl')
    end
  | stringsToTycon ("F" :: sl) =
    (Deftyc, sl)
  | stringsToTycon ("P" :: h :: sl) = 
    (Primtyc (get_bool h), sl)
  | stringsToTycon _ = bug "stringsToTycon"

(*fun stringToTycon s = 
    #1 (stringsToTycon (tokenize s))*)

fun stringToString s = 
    case String.tokens (fn c => c = #"<" orelse c = #">") s of
	[] => ""
      | [s'] => s'
      | _ => bug "stringToString"

fun stringsToLocation (h1 :: h2 :: h3 :: sl) =
    ((stringToString h1, get_int h2, get_int h3), sl)
  | stringsToLocation _ = bug "stringsToLocation"

fun stringsToAccess ("L" :: h :: sl) = 
    (Access.LVAR (get_int h), sl)
  | stringsToAccess ("E" :: h :: sl) = 
    (Access.EXTERN (get_persstamps h), sl)
  | stringsToAccess ("P" :: h :: sl) = 
    let val (acc, sl') = stringsToAccess sl in
	(Access.PATH (acc, get_int h), sl')
    end
  | stringsToAccess _ = 
    bug "stringsToAccess"

fun stringsToVarElemUsage sl = 
    let
	fun f sl = 
	    let val (loc, sl0) = stringsToLocation sl
		val (ty, sl1) = stringsToTy sl0
		val (acc, sl2) = stringsToAccess sl1
	    in
		((loc, ty, acc), sl2)
	    end
    in
	stringsToList f sl
    end

fun stringsToVarElem sl = 
    let
	val (access, sl0) = stringsToAccess sl
	val (name, sl1) = 
	    case sl0 of
		[] => bug "stringsToVarElem.1"
	      | h :: sl1 => (Symbol.varSymbol h, sl1)
	val (parent, sl2) = stringsToAccess sl1
	val (typ, sl3) = stringsToTy sl2
	val (def, sl4) = stringsToLocation sl3
	val (usage, sl5) = stringsToVarElemUsage sl4
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
    ext_fun stringsToVarElem "stringToVar"

fun stringsToKey ("V" :: sl) = 
    let val (acc, sl') = stringsToAccess sl in
	(Var acc, sl')
    end
  | stringsToKey ("S" :: sl) = 
    let val (acc, sl') = stringsToAccess sl in
	(Str acc, sl')
    end
  | stringsToKey ("T" :: sl) =
    let val (st, sl') = stringsToStamp sl in
	(Type st, sl')
    end
  | stringsToKey ("C" :: sl) = 
    let val (st, sl') = stringsToStamp sl in
	( case sl' of 
	      [] => bug "stringsToKey.1"
	    | h :: sl'' => (Cons (st, Symbol.varSymbol h), sl'')
	)
    end
  | stringsToKey ("G" :: sl) = 
    let val (st, sl') = stringsToStamp sl in
	(Sig st, sl')
    end
  | stringsToKey _ = 
    bug "stringsToKey.2"

fun stringsToElements ("D" :: sl) = 
    let fun f (x :: y :: sl0) = 
	    let val (k, sl1) = stringsToKey sl0 in
		((get_int x, get_symbol y, k), sl1)
	    end
	  | f _ = bug "stringsToElements.1"
	val (d, sl') = stringsToList f sl
    in
	(Def d, sl')
    end
  | stringsToElements ("C" :: sl) = 
    let fun f (x :: y :: z :: sl) = 
	    ((get_int x, get_symbol y, get_int z), sl)
	  | f _ = bug "StringToElements.2"
	val (l, sl') = stringsToList f sl
	val (acc, sl'') = stringsToAccess sl'
    in
	(Constraint (l, acc), sl'')
    end
  | stringsToElements ("A" :: sl) = 
    let val (acc, sl') = stringsToAccess sl in
	(Alias acc, sl')
    end
  | stringsToElements _ = bug "stringsToElements.3"

fun stringsToOption _ ("N" :: sl) = (NONE, sl)
  | stringsToOption stringToA ("S" :: sl) = 
    let val (v, sl') = stringToA sl in
	(SOME v, sl')
    end
  | stringsToOption _ _ = bug "stringsToOption"

fun stringsToStrElemUsage sl = 
    stringsToList stringsToLocation sl

fun stringsToStrElem (h :: sl0) = 
    let
	val name = Symbol.strSymbol h
	val (access, sl1) = stringsToAccess sl0
	val (parent, sl2) = stringsToOption stringsToAccess sl1
	val (sign, sl3) = stringsToOption stringsToStamp sl2
	val (def, sl4) = stringsToLocation sl3
	val (elements, sl5) = stringsToElements sl4
	val (usage, sl6) = stringsToStrElemUsage sl5
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
  | stringsToStrElem [] = bug "stringsToStrElem"

val stringToStr = 
    ext_fun stringsToStrElem "stringToStr"

fun stringsToSpecSig ("T" :: sl) = 
    let val (tycon, sl') = stringsToTycon sl in
	(Typ tycon, sl')
    end
  | stringsToSpecSig ("V" :: sl) = 
    let val (ty, sl') = stringsToTy sl in
	(Val ty, sl')
    end
  | stringsToSpecSig ("E" :: sl) =
    let val (ty, sl') = stringsToTy sl in
	(Exception ty, sl')
    end
  | stringsToSpecSig ("N" :: h :: sl) = 
    let val symbol = Symbol.sigSymbol h
	val (stamp, sl') = stringsToStamp sl 
    in
	(NamedStr (symbol, stamp), sl')
    end
  | stringsToSpecSig ("I" :: sl) = 
    let val (el, sl') = stringsToElementsSig sl in
	(InlineStr el, sl')
    end
  | stringsToSpecSig _ = bug "stringsToSpecSig"

and stringsToElementsSig sl = 
    let fun f (h :: sl) = 
	    let val symbol = get_symbol h
		val (spec_sig, sl') = stringsToSpecSig sl
	    in
		((symbol, spec_sig), sl')
	    end
	  | f [] = bug "stringsToElementsSig"
    in
	stringsToList f sl
    end

fun stringsToSigElemUsage sl = 
    let
	fun f sl = 
	    let val (loc, sl') = stringsToLocation sl
		val (symbol, sl'') = 
		    case sl' of
			[] => bug "stringsToSigElemUsage"
		      | h :: sl'' => (Symbol.sigSymbol h, sl'')
	    in
		((loc, symbol), sl'')
	    end
    in
	stringsToList f sl
    end

fun stringsToSigElemAlias sl = 
    stringsToSigElemUsage sl

fun stringsToSigElem (h :: sl) = 
    let val name = Symbol.sigSymbol h
	val (stamp, sl0) = stringsToStamp sl
	val (inferred, sl1) = 
	    case sl0 of
		[] => bug "stringsToSigElem.1"
	      | h :: sl1 => (get_bool h, sl1)
	val (def, sl2) = stringsToLocation sl1
	val (elements, sl3) = stringsToElementsSig sl2
	val (alias, sl4) = stringsToSigElemAlias sl3
	val (usage, sl5) = stringsToSigElemUsage sl4
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
  | stringsToSigElem [] = bug "stringsToSigElem.2"

val stringToSig = 
    ext_fun stringsToSigElem "stringToSig"

fun stringsToTypeElemUsage sl = 
    stringsToList stringsToLocation sl

fun stringsToTypeElem sl = 
    let val (tycon, sl0) = stringsToTycon sl
	val (stamp, sl1) = stringsToStamp sl0
	val (name, sl2) = 
	    case sl1 of
		[] => bug "stringsToTypeElem.1"
	      | h :: sl2 => (Symbol.tycSymbol h, sl2)
	val (def, sl3) = stringsToLocation sl2
	val (usage, sl4) = stringsToTypeElemUsage sl3
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
    ext_fun stringsToTypeElem "stringToType"

fun stringsToConsElemUsage sl = 
    let fun f sl = 
	    let val (loc, sl') = stringsToLocation sl
		val (ty, sl'') = stringsToTy sl'
	    in
		((loc, ty), sl'')
	    end
    in
	stringsToList f sl
    end

fun stringsToConsElem (h :: sl) = 
    let val name = Symbol.varSymbol h
	val (dataty, sl0) = stringsToStamp sl
	val (def, sl1) = stringsToLocation sl0
	val (ty, sl2) = stringsToTy sl1
	val (usage, sl3) = stringsToConsElemUsage sl2
    in
	( { name = name,
	    dataty = dataty,
	    def = def,
	    ty = ty,
	    usage = ref usage
	  }, sl3)
    end
  | stringsToConsElem [] = 
    bug "stringsToConsElem"

val stringToCons = 
    ext_fun stringsToConsElem "stringToCons"

fun stringsToExtElem ("v" :: sl) = 
    let	val (access, sl0) = stringsToAccess sl
	val (usage, sl1) = stringsToVarElemUsage sl0
    in
	(ExtVar{access = access, usage = ref usage}, sl1)
    end
  | stringsToExtElem ("s" :: sl) = 
    let	val (access, sl0) = stringsToAccess sl
	val (usage, sl1) = stringsToStrElemUsage sl0
    in
	(ExtStr{access = access, usage = ref usage}, sl1)
    end
  | stringsToExtElem ("t" :: sl) =
    let	val (stamp, sl0) = stringsToStamp sl
	val (usage, sl1) = stringsToTypeElemUsage sl0
    in
	(ExtType{stamp = stamp, usage = ref usage}, sl1)
    end
  | stringsToExtElem ("c" :: h ::sl) = 
    let val name = Symbol.varSymbol h
	val (stamp, sl0) = stringsToStamp sl
	val (usage, sl1) = stringsToConsElemUsage sl0
    in
	(ExtCons{name = name, stamp = stamp, usage = ref usage}, sl1)
    end
  | stringsToExtElem ("g" :: sl) = 
    let val (stamp, sl0) = stringsToStamp sl
	val (usage, sl1) = stringsToSigElemUsage sl0
    in
	(ExtSig{stamp = stamp, usage = ref usage}, sl1)
    end
  | stringsToExtElem _ =
    bug "stringsToExtElem"

val stringToExt = 
    ext_fun stringsToExtElem "stringToExt"

fun stringsToLvarExtElems sl =
    let val (acc1, sl0) = stringsToAccess sl
	val (acc2, sl1) = stringsToAccess sl0
    in
	((acc1, acc2), sl1)
    end

val stringToLvarExt = 
    ext_fun stringsToLvarExtElems "stringToLvarExt"

fun stringsToPidOption sl = 
    stringsToOption stringsToPersstamps sl

fun stringToPidOption s = 
    case stringsToPidOption (tokenize s) of
	(v, []) => v
      | (_, rem) => (print (concat rem); print "\n"; bug "stringToPidOption")

fun stringsToOccurrence (h :: sl) = 
    let val symbol = get_symbol h
	val (location, sl') = stringsToLocation sl
    in
	((symbol,location), sl')
    end
  | stringsToOccurrence [] = 
    bug "stringsToOccurrence"

val stringToOccurrenceList = 
    ext_fun stringsToOccurrence "stringsToOccurrenceList"

end (* structure StringToTy*)
end (* local *)
