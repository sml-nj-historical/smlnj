(* Copyright 1989 by AT&T Bell Laboratories *)
structure Intset : sig type intset
		        val new : unit -> intset
		        val add : intset -> int -> unit
			val rmv : intset -> int -> unit
			val mem : intset -> int -> bool
		   end =

struct
  open Array List
  infix 9 sub

  val p = 211
  type intset = int list array
  fun new () = array(p,nil : int list)
  fun add a i = let val index = i mod p in update(a,index,i::(a sub index)) end
  fun mem a i = exists (fn j => j=i) (a sub (i mod p))
  fun rmv a i = let fun f (j::r) = if i=j then f r else j :: f r
	              | f nil = nil
		    val index = i mod p
		 in update(a,index, f(a sub index))
		end
(*
  type intset = unit Intmap.intmap
  exception NO
  fun new () =  Intmap.new(32, NO)
  fun add a i = Intmap.add a (i,())
  fun mem a i = (Intmap.map a i; true) handle NO => false
  fun rmv a i = Intmap.rem a i
*)
end


(*
 * $Log: intset.sml,v $
 * Revision 1.1.1.1  1997/01/14  01:38:48  george
 *   Version 109.24
 *
 *)
