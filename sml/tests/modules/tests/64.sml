(* 64.sml *)
(* keywords: sharing, functor *)

(* Doligez *)
(* sharing with a free structure thinned by functor application *)
(* status : should work, does not work in 66, works in dd67 *)

functor F (X : sig end) = X;

structure A =
  struct type t = int
  end;

structure B = F (A);

signature S =
  sig structure C : sig type t end
      sharing C = B
  end;

functor F (X : S) =
  (* we have X.C = B = F(A) = A *)
  struct val x : X.C.t = 3
  end;
