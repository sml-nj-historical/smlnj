(* 96.sml *)
(* should work in SML 97 *)

structure A = 
struct
end

structure B =
struct
end

signature S1 =
sig
  structure C : sig end = A
end

signature S2 =
sig
  structure C : sig end = B
end

signature S3 =
sig
  structure D : S1
  structure E : S2
  sharing D = E
end

functor F(X:S3) = struct end
