(* tytostring.sml *)

local
    open Ens_types2
in
structure TyToString : TYTOSTRING =
struct

fun bug x = ErrorMsg.impossible ("TyToString: " ^ x)

val flatten = String.concatWith " "

fun listToStrings aToStrings l = 
    Int.toString(length l) :: List.concat(map aToStrings l)

fun ext_fun f l = 
    flatten (listToStrings f l)

fun stampToStrings stamp =
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

fun tycToStrings (General(stamp,path)) = 
    "G" :: stampToStrings stamp @ [pathToString path]
  | tycToStrings (Record labels) = ["R", labelsToString labels]
  | tycToStrings (Path path) = ["P", pathToString path]

fun tyToStrings (Conty(stubtyc,args)) = 
    "C" :: tycToStrings stubtyc @ tyListToStrings args
  | tyToStrings (Ibound index) = 
    ["I", Int.toString index]
  | tyToStrings (Lbound{index,depth}) = 
    ["L", Int.toString index, Int.toString depth]
  | tyToStrings (Ubound s) = 
    ["U", Symbol.symbolToString s]
  | tyToStrings (Poly{arity,body}) = 
    ["P", Int.toString arity] @ tyToStrings body

and tyListToStrings l = 
    listToStrings tyToStrings l

(*fun tyToString ty = flatten (tyToStrings ty)*)

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

(*fun tyconToString tycon = flatten (tyconToStrings tycon)*)

fun stringToString s = "<" ^ s ^ ">"

fun locationToStrings (f, r1, r2) = 
    [stringToString f, Int.toString r1, Int.toString r2]

fun accessToStrings (Access.LVAR i) = 
    ["L", Int.toString i]
  | accessToStrings (Access.EXTERN pstamp) = 
    ["E", PersStamps.toHex pstamp]
  | accessToStrings (Access.PATH (acc, i)) = 
    "P" :: Int.toString i :: accessToStrings acc
  | accessToStrings (Access.NO_ACCESS) = 
    bug "accessToStrings"

fun varElemUsageToStrings usage = 
    listToStrings 
	(fn(x, y, z) =>locationToStrings x @ tyToStrings y @ accessToStrings z)
	(!usage)
    
fun varElemToStrings {access, name, parent, typ, def, usage} = 
    accessToStrings access @
    Symbol.name name ::
    accessToStrings parent @
    tyToStrings typ @
    locationToStrings def @
    varElemUsageToStrings usage

val varToString = 
    ext_fun varElemToStrings

fun keyToStrings (Var acc) = 
    "V" :: accessToStrings acc
  | keyToStrings (Str acc) = 
    "S" :: accessToStrings acc
  | keyToStrings (Type s) = 
    "T" :: stampToStrings s
  | keyToStrings (Cons (stamp, symbol)) = 
    "C" :: stampToStrings stamp @ [Symbol.name symbol]
  | keyToStrings (Sig s) = 
    "G" :: stampToStrings s

fun elementsToStrings (Def l) = 
    "D" :: listToStrings 
	       (fn (x, y, z) => Int.toString x :: 
				Symbol.symbolToString y ::
				keyToStrings z) 
	       l
  | elementsToStrings (Constraint (l, acc)) = 
    "C" :: listToStrings 
	       (fn (x, y, z) => Int.toString x :: 
				Symbol.symbolToString y ::
				[Int.toString z]) 
	       l @
    accessToStrings acc
  | elementsToStrings (Alias acc) = 
    "A" :: accessToStrings acc

fun optionToStrings _ NONE = ["N"]
  | optionToStrings aToString (SOME a) = "S" :: aToString a

fun strElemUsageToStrings usage = 
    listToStrings locationToStrings (!usage)

fun strElemToStrings {name, access, parent, sign, def, elements, usage} =
    Symbol.name name ::
    accessToStrings access @
    optionToStrings accessToStrings parent @
    optionToStrings stampToStrings sign @
    locationToStrings def @
    elementsToStrings elements @
    strElemUsageToStrings usage

val strToString = 
    ext_fun strElemToStrings

fun specSigToStrings (Typ tycon) = 
    "T" :: tyconToStrings tycon
  | specSigToStrings (Val ty) = 
    "V" :: tyToStrings ty
  | specSigToStrings (Exception ty) =
    "E" :: tyToStrings ty
  | specSigToStrings (NamedStr (symbol, stamp)) =
    "N" :: Symbol.name symbol :: stampToStrings stamp
  | specSigToStrings (InlineStr elements) = 
    "I" :: elementsSigToStrings elements

and elementsSigToStrings l = 
    listToStrings (fn (x,y) => Symbol.symbolToString x :: specSigToStrings y) l

fun sigElemUsageToStrings usage = 
    listToStrings (fn (x, y) => locationToStrings x @ [Symbol.name y]) (!usage)

fun sigElemAliasToStrings alias = 
    sigElemUsageToStrings alias

fun sigElemToStrings {name, stamp, inferred, def, elements, alias, usage} = 
    Symbol.name name ::
    stampToStrings stamp @ 
    boolToString inferred :: 
    locationToStrings def @ 
    elementsSigToStrings elements @
    sigElemAliasToStrings alias @
    sigElemUsageToStrings usage

val sigToString =
    ext_fun sigElemToStrings

fun typeElemUsageToStrings usage = 
    listToStrings locationToStrings (!usage)

fun typeElemToStrings {tycon, stamp, name, def, usage} = 
    tyconToStrings tycon @ 
    stampToStrings stamp @
    Symbol.name name :: 
    locationToStrings def @
    typeElemUsageToStrings usage

val typeToString = 
    ext_fun typeElemToStrings

fun consElemUsageToStrings usage = 
    listToStrings (fn (x, y) => locationToStrings x @ tyToStrings y) (!usage)

fun consElemToStrings {name, dataty, def, ty, usage} = 
    Symbol.name name ::
    stampToStrings dataty @
    locationToStrings def @
    tyToStrings ty @
    consElemUsageToStrings usage

val consToString = 
    ext_fun consElemToStrings

fun extElemToStrings (ExtVar {access, usage}) = 
    "v" :: accessToStrings access @ varElemUsageToStrings usage
  | extElemToStrings (ExtStr {access, usage}) = 
    "s" :: accessToStrings access @ strElemUsageToStrings usage
  | extElemToStrings (ExtType {stamp, usage}) = 
    "t" :: stampToStrings stamp @ typeElemUsageToStrings usage
  | extElemToStrings (ExtCons {name, stamp, usage}) = 
    "c" :: Symbol.name name :: stampToStrings stamp @ 
    consElemUsageToStrings usage
  | extElemToStrings (ExtSig {stamp, usage}) = 
    "g" :: stampToStrings stamp @ sigElemUsageToStrings usage

val extToString = 
    ext_fun extElemToStrings

fun lvarExtElemToStrings (acc1, acc2) = 
    accessToStrings acc1 @ accessToStrings acc2

val lvarExtToString  = 
    ext_fun lvarExtElemToStrings

fun pidOptionToStrings po = 
    optionToStrings (fn x => [PersStamps.toHex x]) po

fun pidOptionToString po = 
    flatten (pidOptionToStrings po)

end (* structure TyToString *)
end (* end local *)
