(* bug1354.3.sml *)

signature S1 =
sig
  structure A : sig end
end;

signature S2 =
sig
  structure B : S1
  structure C : S1 = B
end;

signature S3 =
sig
  structure E : S2
  structure F : S2 = E
end;
