(* ------------------------  tconst.sml: ---------------------- *)
import "globals";

signature TCONST =
sig
  type t
  val from_string: string -> t
end

functor TConstFun((*structure Globals:GLOBALS*)): TCONST =
struct
    exception IllegalTConst of string
    type t = string
    fun member x [] = false
      | member x (y::l) = (x=y) orelse (member x l)
    fun from_string s = if not (member s ["int", "real", "bool"])
		       then raise IllegalTConst(s)
		       else s
end

