(* 
 * This file tests translation of opaque ascription of type definitions in 
 * nested structures.
 * It also appears to set off the negative index error for TC_VARs
 * The Tycpath for type t should be a TVar. 
 *)

functor F(structure A: sig structure B: sig type t end end) = struct end

functor G() = 
struct 
structure M = F(struct structure A = 
		         struct 
			   structure B = 
				  struct type t = unit end 
			 end :> sig structure B : sig type t end end
		end)
end