(* test59.sml *)
(* keywords: sharing, functor *)

(* Doligez *)
(* sharing with a free structure via a free thinned version of the structure *)
(* status : should work, does not work in 66, works in dd67 *)

structure A =
  struct type t = int
  end;

structure B : sig end = A;

signature S1 =
  sig structure C : sig type t end
      sharing C = B
  end;

functor F (X : S1) =
  (* we have X.C = B = A and A.t = int *)
  struct val x : X.C.t = 3
  end;
