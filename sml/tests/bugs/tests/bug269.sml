(* bug269.sml *)
(* failure in abstractBody with embedded signature *)

functor F() =
struct
  datatype d = D
  structure A : sig type s val x : s * d end =
      struct
        datatype s = MKs
	val x = (MKs,D)
      end
end;

structure B = F();

val (_,B.D) = B.A.x;

