(* bug1354.4.sml *)

signature S1 =
sig
  type t
end;

signature S2 =
sig
  structure A : S1
  structure B : S1 = A
end;

signature S3 =
sig
  structure C : S2
  structure D : S2 = C
end;
