(* bug1270.1.sml *)

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

functor F (P: sig structure X: S3 end) =
struct
  type s = P.X.u
end;

functor G (U: S3) (V: S2) : S2 =  (* ": S2" necessary *)
struct
  structure A = F (struct structure X = U end)
  val x = U.y
end;
