(* test1.sml *)
(* keywords: equality, sharing, functor *)

(* contributed by Simon Finn *)
(* should work *)

signature S1 =
sig
  eqtype t
  val x : t
end;

signature S2 =
sig
  structure A : sig end
  structure C : sig structure A : S1 end
  sharing A = C.A
end;

functor F(structure A:S1
          structure B:S2
          sharing A = B.A)  =
struct
  val y = (A.x = B.C.A.x)
end;
