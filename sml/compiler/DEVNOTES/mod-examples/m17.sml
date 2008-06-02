
(* 
 * This example is yet another test of deBruijn indexing 
 * possibilities.
 *)

signature S =
    sig
	type t
	
	val y : t -> int
    end

functor G (structure A : S) = 
struct
  open A
end

 
functor F(structure T : S) =
struct
  structure M = G(structure A = T)
  val x = 1
end
 