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

fun stampToString stamp =
    let fun fresh n = []
	fun special s = ["SS", s]
	fun global {pid,cnt} = ["GS", PersStamps.toHex pid, Int.toString cnt]
    in 
	Stamps.Case 
	    (Stamps.newConverter()) 
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
  | tyToStrings (Ibound{index,depth}) = 
    ["I", Int.toString index, Int.toString depth]
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

fun varElemToString {access, name, parent, typ, def, usage} = 
    accessToString access @
    Symbol.name name ::
    accessToString parent @
    tyToStrings typ @
    locationToString def @
    listToString 
	(fn(x, y, z) => locationToString x @ tyToStrings y @ accessToString z) 
	(!usage)

fun varToString l = 
    listToString varElemToString l

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

fun strElemToString {name, access, parent, sign, def, elements, usage} =
    Symbol.name name ::
    accessToString access @
    optionToString accessToString parent @
    optionToString stampToString sign @
    locationToString def @
    elementsToString elements @
    listToString locationToString (!usage)

fun strToString l = 
    listToString strElemToString l

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

fun sigElemToString {name, stamp, inferred, def, elements, alias, usage} = 
    Symbol.name name ::
    stampToString stamp @ 
    boolToString inferred :: 
    locationToString def @ 
    elementsSigToString elements @
    listToString (fn (x, y) => locationToString x @ [Symbol.name y]) (!alias) @
    listToString (fn (x, y) => locationToString x @ [Symbol.name y]) (!usage)

fun sigToString l = 
    listToString sigElemToString l

fun allToStrings (var, str, sign) = 
    varToString var @ strToString str @ sigToString sign

fun allToString t = 
    flatten (allToStrings t)

end (* structure TyToString *)
end (* end local *)
