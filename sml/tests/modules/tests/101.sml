(* 101.sml *)

signature S =
sig
  structure A : sig type x end
  structure B : sig
		  structure A : sig type y end
		  type t
		  sharing type t = A.x
		end
end
