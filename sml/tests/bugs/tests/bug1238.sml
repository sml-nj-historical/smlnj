(* bug1238.sml *)

signature M1 =
sig
  type t
end;

signature M2 =
sig
  type t
end;

signature S =
sig
  structure M  : M1 
end;

(* ------------ this one works ---------------------- *)
functor Good(structure D : M2
	     structure Mu : S where type  M.t = D.t) 
    = struct end;

(* ------------ this one fails ---------------------- *)
functor Bad(structure D : M2 (* D and Mu.M have different sigs *)
	    structure Mu : S where M = D) 
    = struct end;
