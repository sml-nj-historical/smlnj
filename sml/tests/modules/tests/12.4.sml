(* 12.4.sml *)
(* keywords: functor *)

(* derived from mlyacc.sml (old version) *)

functor F () =
struct
  type u = int
end;

functor G () =
struct
  structure E = F()
end;
		    
structure B = G()

val foo =  fn (x : B.E.u) => ()
