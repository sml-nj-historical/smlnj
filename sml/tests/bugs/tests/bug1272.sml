(* bug1272.sml *)

signature Z = sig end;

signature S =
sig
  structure A: Z
end;

signature T1 =
sig
  structure B: S
  structure A : Z = B.A
end;

signature T2 =
sig
  structure B: S
  structure A : Z = B.A
end;

signature V =
sig
  structure C: T1
  structure D: T2
  sharing C = D
end;
