(* bug727.sml *)
(* 727. printing sharing constraints in signature *)

functor F (X : sig end) = X;

structure A =
  struct type t = int
  end;

structure B = F (A);

signature S =
  sig structure C : sig type t end
      sharing C = B
  end;
