(* show that we may need to relativize structures in the instantiation array
   of an EMBEDDED structure that are not substructures of the EMBEDDED
   structure.

   This goes with test7.sml. *)
  
signature S0 = sig type t end

functor F(A : sig type t val x : t end) =
struct
local
   structure A  : sig structure B : S0
	               structure C : sig val x : B.t end
	          end =
	struct
	   structure B = struct type t = A.t end
	   structure C = struct val x = A.x end
        end
in
   structure D = A.C
end
end

structure S = F(struct type t=int val x=5 end)
	    
structure A = struct val x : int = S.D.x end
