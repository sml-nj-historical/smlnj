(* 12.sml *)
(* keywords: functor *)

(* derived from mlyacc.sml (old version) *)

signature SET =
sig
  type r
end;

signature GRAPH =
sig
  structure E : SET
end;

signature GRAM =
sig
  datatype s = TERM
end;

signature LGRAPH =
sig
  structure L : GRAPH
end;

functor F (X : sig type t end) : SET =
struct
  type r = X.t
end;

functor G (Y : sig type u end) : GRAPH =
struct
  type ge = Y.u list
  structure E = F(struct type t = ge end)
end;
		    
functor H(Z: GRAM) : LGRAPH =
struct
  structure L = G (struct type u = Z.s end)
end;

structure A = struct datatype s = TERM end;
structure B = H(A);

val foo =  fn (x::_ : B.L.E.r) => case x of A.TERM => ();
