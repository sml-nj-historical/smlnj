(* Copyright 1996 by AT&T Bell Laboratories *)
(* entpath.sml *)

signature ENT_PATH =
sig

  type entVar
  type entPath = entVar list

  val eqEntVar : entVar * entVar -> bool
  val eqEntPath : entPath * entPath -> bool

  val cmpEntVar : entVar * entVar -> order
  val cmpEntPath : entPath * entPath -> order

  val nullEntPath : entPath -> bool
  val entVarToString : entVar -> string
  val entPathToString : entPath -> string

  val bogusEntVar : entVar

  structure EvDict : BINARY_DICT

end  (* signature ENT_PATH *)


structure EntPath : ENT_PATH =
struct

local
  structure ST = Stamps
in

type entVar = ST.stamp

type entPath = entVar list
(* entPath has entVars in direct order, outer first *)


val eqEntVar = ST.eq

fun eqEntPath (ep1,ep2) =
    let fun all(v::l,u::m) = eqEntVar(v,u) andalso all(l,m)
	  | all(nil,nil) = true
	  | all _ = false
     in all(ep1,ep2)
    end

val cmpEntVar = ST.cmp

fun cmpEntPath (ep1, ep2) = 
  let fun f(a::ar, b::br) =
            (case ST.cmp(a,b) of EQUAL => f(ar,br) | z => z)
        | f(a::ar, nil) = GREATER
        | f(nil, b::br) = LESS
        | f(nil,nil) = EQUAL
   in f(ep1,ep2)
  end

structure EvDict = BinaryDict(struct type ord_key = entVar 
                                     val cmpKey = cmpEntVar
                              end)

(* ListPair.all didn't cut it because it doesn't require lists of equal length
    length ep1 = length ep2 andalso
    ListPair.all eqEntVar (ep1, ep2)
*)

fun nullEntPath(ep: entPath) = List.null ep

fun entVarToString (v: entVar) = ST.stampToShortString v

fun entPathToString ([]: entPath) = "[]"
  | entPathToString (x::xs) =
      let val rest = foldr (fn (y,l) => ","::(ST.stampToShortString y)::l) ["]"] xs
       in String.concat("[" :: (ST.stampToShortString x) :: rest)
      end

val bogusEntVar = ST.special "bogusEntVar"

end (* local *)
end (* structure EntPath *)

(*
 * $Log$
 *)
