(* 12.2.sml *)
(* keywords: functor *)

(* derived from mlyacc.sml (old version) *)

functor F (X : sig type u end)  =
struct
  type u = X.u
end;

functor G (Y : sig type u end) =
struct
  structure E = F(Y)
end;
		    
structure B = G(struct type u = int end);

val foo =  fn (x : B.E.u) => ()
