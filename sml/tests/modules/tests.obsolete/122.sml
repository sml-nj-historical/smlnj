(* test nested opens *)

signature S1 =
sig
   type t
end

signature S2 =
sig
   structure A : S1
end

signature S3 =
sig
  structure B : S2
  open B
  open A
  val x : t
  sharing type t = int
end
