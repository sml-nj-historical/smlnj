(* test60.sml *)
(* keywords: sharing, functor *)

(* Doligez *)
(* sharing with a bound structure via substructure sharing *)
(* status : should work, does not work in 66, works in dd67 *)

signature S1 =
  sig structure A : sig eqtype t val x : t end
      structure B : sig end
      sharing A = B
  end;

signature S2 =
  sig structure C : S1
      structure D : sig eqtype t val y : t end
      sharing C.B = D
  end;

functor F (X : S2) =
  (* we have X.D = X.C.B = X.C.A *)
  struct val foo = (X.D.y = X.C.A.x)
  end;

