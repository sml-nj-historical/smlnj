(* tytostring.sml *)

local
    open Ens_types2
in
structure TyToString : TYTOSTRING =
struct

fun bug x = ErrorMsg.impossible ("TyToString: " ^ x)

val flatten = String.concatWith " "

fun listToString aToString l = 
    Int.toString(length l) :: List.concat(map aToString l)

fun ext_fun f l = 
    flatten (listToString f l)

fun stampToString stamp =
    let fun fresh n = ["FS", Int.toString n]
	fun special s = ["SS", s]
	fun global {pid,cnt} = ["GS", PersStamps.toHex pid, Int.toString cnt]
    in 
	Stamps.Case'
	    stamp
	    {fresh=fresh,special=special,global=global} 
    end

fun labelsToString labs =
     let fun f [s] = [Symbol.name s, ">"]
	   | f (a::r) = Symbol.name a :: "." :: f r
	   | f nil = [">"]
      in concat("<" :: f labs)
     end

fun pathToString (InvPath.IPATH (h::q)) =
    String.concatWith 
	"." 
	(Symbol.symbolToString h :: List.map Symbol.name q)
  | pathToString (InvPath.IPATH []) = 
    bug "pathToString"

fun tycToString (General(stamp,path)) = 
    "G" :: stampToString stamp @ [pathToString path]
  | tycToString (Record labels) = ["R", labelsToString labels]
  | tycToString (Path path) = ["P", pathToString path]

fun tyToStrings (Conty(stubtyc,args)) = 
    "C" :: tycToString stubtyc @ tyListToString args
  | tyToStrings (Ibound index) = 
    ["I", Int.toString index]
  | tyToStrings (Lbound{index,depth}) = 
    ["L", Int.toString index, Int.toString depth]
  | tyToStrings (Ubound s) = 
    ["U", Symbol.symbolToString s]
  | tyToStrings (Poly{arity,body}) = 
    ["P", Int.toString arity] @ tyToStrings body

and tyListToString l = 
    listToString tyToStrings l

fun tyToString ty = flatten (tyToStrings ty)

fun boolToString b = 
    if b then "t" else "f"

fun tyconToStrings (Datatype (b, sl)) = 
    ["D", boolToString b, labelsToString sl]
  | tyconToStrings (Abstract sl) = 
    ["A", labelsToString sl]
  | tyconToStrings Deftyc =
    ["F"]
  | tyconToStrings (Primtyc b) =
    ["P", boolToString b]

fun tyconToString tycon = flatten (tyconToStrings tycon)

fun stringToString s = "<" ^ s ^ ">"

fun locationToString (f, r1, r2) = 
    [stringToString f, Int.toString r1, Int.toString r2]

fun accessToString (Access.LVAR i) = 
    ["L", Int.toString i]
  | accessToString (Access.EXTERN pstamp) = 
    ["E", PersStamps.toHex pstamp]
  | accessToString (Access.PATH (acc, i)) = 
    "P" :: Int.toString i :: accessToString acc
  | accessToString (Access.NO_ACCESS) = 
    bug "accessToString"

fun varElemUsageToString usage = 
    listToString 
	(fn(x, y, z) => locationToString x @ tyToStrings y @ accessToString z) 
	(!usage)
    
fun varElemToString {access, name, parent, typ, def, usage} = 
    accessToString access @
    Symbol.name name ::
    accessToString parent @
    tyToStrings typ @
    locationToString def @
    varElemUsageToString usage

val varToString = 
    ext_fun varElemToString

fun keyToString (Var acc) = 
    "V" :: accessToString acc
  | keyToString (Str acc) = 
    "S" :: accessToString acc
  | keyToString (Type s) = 
    "T" :: stampToString s
  | keyToString (Cons (stamp, symbol)) = 
    "C" :: stampToString stamp @ [Symbol.name symbol]
  | keyToString (Sig s) = 
    "G" :: stampToString s

fun elementsToString (Def l) = 
    "D" :: listToString 
	       (fn (x, y, z) => Int.toString x :: 
				Symbol.symbolToString y ::
				keyToString z) 
	       l
  | elementsToString (Constraint (l, acc)) = 
    "C" :: listToString 
	       (fn (x, y, z) => Int.toString x :: 
				Symbol.symbolToString y ::
				[Int.toString z]) 
	       l @
    accessToString acc
  | elementsToString (Alias acc) = 
    "A" :: accessToString acc

fun optionToString _ NONE = ["N"]
  | optionToString aToString (SOME a) = "S" :: aToString a

fun strElemUsageToString usage = 
    listToString locationToString (!usage)

fun strElemToString {name, access, parent, sign, def, elements, usage} =
    Symbol.name name ::
    accessToString access @
    optionToString accessToString parent @
    optionToString stampToString sign @
    locationToString def @
    elementsToString elements @
    strElemUsageToString usage

val strToString = 
    ext_fun strElemToString

fun specSigToString (Typ tycon) = 
    "T" :: tyconToStrings tycon
  | specSigToString (Val ty) = 
    "V" :: tyToStrings ty
  | specSigToString (Exception ty) =
    "E" :: tyToStrings ty
  | specSigToString (NamedStr (symbol, stamp)) =
    "N" :: Symbol.name symbol :: stampToString stamp
  | specSigToString (InlineStr elements) = 
    "I" :: elementsSigToString elements

and elementsSigToString l = 
    listToString (fn (x, y) => Symbol.symbolToString x :: specSigToString y) l

fun sigElemUsageToString usage = 
    listToString (fn (x, y) => locationToString x @ [Symbol.name y]) (!usage)

fun sigElemAliasToString alias = 
    sigElemUsageToString alias

fun sigElemToString {name, stamp, inferred, def, elements, alias, usage} = 
    Symbol.name name ::
    stampToString stamp @ 
    boolToString inferred :: 
    locationToString def @ 
    elementsSigToString elements @
    sigElemAliasToString alias @
    sigElemUsageToString usage

val sigToString =
    ext_fun sigElemToString

fun typeElemUsageToString usage = 
    listToString locationToString (!usage)

fun typeElemToString {tycon, stamp, name, def, usage} = 
    tyconToStrings tycon @ 
    stampToString stamp @
    Symbol.name name :: 
    locationToString def @
    typeElemUsageToString usage

val typeToString = 
    ext_fun typeElemToString

fun consElemUsageToString usage = 
    listToString (fn (x, y) => locationToString x @ tyToStrings y) (!usage)

fun consElemToString {name, dataty, def, ty, usage} = 
    Symbol.name name ::
    stampToString dataty @
    locationToString def @
    tyToStrings ty @
    consElemUsageToString usage

val consToString = 
    ext_fun consElemToString

fun extElemToString (ExtVar {access, usage}) = 
    "v" :: accessToString access @ varElemUsageToString usage
  | extElemToString (ExtStr {access, usage}) = 
    "s" :: accessToString access @ strElemUsageToString usage
  | extElemToString (ExtType {stamp, usage}) = 
    "t" :: stampToString stamp @ typeElemUsageToString usage
  | extElemToString (ExtCons {name, stamp, usage}) = 
    "c" :: Symbol.name name :: stampToString stamp @ 
    consElemUsageToString usage
  | extElemToString (ExtSig {stamp, usage}) = 
    "g" :: stampToString stamp @ sigElemUsageToString usage

val extToString = 
    ext_fun extElemToString

fun lvarExtElemToString (acc1, acc2) = 
    accessToString acc1 @ accessToString acc2

val lvarExtToString  = 
    ext_fun lvarExtElemToString

end (* structure TyToString *)
end (* end local *)
