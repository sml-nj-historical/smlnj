(* 91.sml *)

signature S0 =
sig
  type u
end

structure D : S0 = 
struct
  datatype u = U
end

functor F2(A : sig 
	        structure C : S0 = D
	       end) : sig end =
struct
end


