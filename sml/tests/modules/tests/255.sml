(* b922.3.sml *)

signature S2 =
sig
  structure A : sig type s end
  val x : A.s
end;

signature S3 =
sig
  type u
  val y : u
end;

functor F (X: S3) =
struct
  type s = X.u
end;

functor G (U: S3) (V: S2) : S2 =  (* ": S2" necessary *)
struct
  structure A = F(U)
  val x = U.y
end;
