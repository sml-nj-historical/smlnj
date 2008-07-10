(* tytostring.sml *)

structure TyToString : TYTOSTRING =
struct

fun tyToStrings (Conty(stubtyc,args)) = "C" :: tycToString stubtyc "" tyListToString args
  | tyToStrings (Ibound{index,depth}) = ["I", Int.toString index, Int.toString depth]
  | tyToStrings (Ubound s) = ["U", Symbol.symbolToString s]
  | tyToStrings (Poly{arity,body) = ["P", Int.toString arity] @ tyToStrings body

fun tyListToString [] = "0"
  | tyListToString l = Int.toString(length l) :: List.concat(map tyToStrings l)

fun tycToString (General(stamp,path)) = ["G", stampToString stamp, pathToString path]
  | tycToString (Record labels) = ["R", labelsToString labels]

fun stampToString (stamp) =
    let fun fresh n = []
	fun special s = ["SS", s]
	fun global {pid,cnt} = ["GS", Persstamps.toHex pid, Int.toString cnt]
    in Stamps.Case (Stamp.newConverter()) {fresh=fresh,special=special,global=global} stamp
    end

fun pathToString (IPATH p: path) =
     let fun f [s] = [Symbol.symbolToString s]
	   | f (a::r) = Symbol.name a :: "." :: f r
	   | f nil = ErrorMsg.impossible "pathToString"
      in concat(f p)
     end

fun labelsToString (labs) =
     let fun f [s] = [Symbol.name s, ">"]
	   | f (a::r) = Symbol.name a :: "." :: f r
	   | f nil = [">"]
      in concat("<" :: f labs)
     end

fun tyToString ty = String.concatWith " " tyToStrings ty

end (* structure TyToString *)


