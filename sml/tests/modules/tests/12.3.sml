(* 12.3.sml *)
(* keywords: functor *)

(* derived from mlyacc.sml (old version) *)

functor G (Y : sig type u end) =
struct
  structure E = struct type u = Y.u end
end;
		    
structure B = G(struct type u = int end);

val foo =  fn (x : B.E.u) => ()
