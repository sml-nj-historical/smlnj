(* 101.1.sml *)

signature S =
sig
  structure A : sig structure X : sig end end
  structure B : sig
		  structure A : sig end
		  structure B : sig end
		  sharing B = A.X
		end
end
