(* bug1270.sml *)

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

functor F (structure X: S3) =  (* "structure" necessary *)
struct
  type s = X.u
end;

functor G (U: S3) (V: S2) : S2 =  (* ": S2" necessary *)
struct
  structure A = F (structure X = U)
  val x = U.y
end;

(* bug1270.sml *)

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

functor F (structure X: S3) =  (* "structure" necessary *)
struct
  type s = X.u
end;

functor G (U: S3) (V: S2) : S2 =  (* ": S2" necessary *)
struct
  structure A = F (structure X = U)
  val x = U.y
end;

