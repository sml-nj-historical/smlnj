(* test66.sml *)
(* keywords: sharing, functor *)

(* Doligez *)
(* sharing two types indirectly *)
(* status : should work (???), does not work in 66, does not work in dd67 *)

signature S1 = sig type t val x : t end;

signature S2 = sig val x : bool end;

signature S =
  sig structure A : S1
      structure B : S2
      sharing A = B
  end;

functor F (X : S) =
  struct val x : X.A.t = true
  end;
