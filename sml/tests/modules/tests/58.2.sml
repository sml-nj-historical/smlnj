(* 58.2.sml *)
(* keywords: sharing, functor *)

(* Doligez *)
(* sharing with a free structure via a substructure *)
(* status : should work, does not work in 66, works in dd67 *)

structure A =
  struct type t = int
  end;

signature S1 =
  sig structure B : sig end = A
  end;

signature S2 =
  sig structure C : S1
      structure D : sig type t end = C.B
  end;

functor F (X : S2) =
  (* we have X.D = X.C.B = A and A.t = int *)
  struct val x : X.D.t = 3
  end;
