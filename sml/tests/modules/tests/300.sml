(* 300.sml *)
(* related to bug 1445 *)

signature S1 =
sig
  type t
end;

signature S2 =
sig
  structure T : S1
  datatype r = Kr of T.t
end;

signature S3 =
sig
  type s
  datatype t = Kt of s
end;

signature S4 =
sig
  structure C : S2
  structure D : S2 where T = C.T
  structure E : S3
  sharing type E.t = C.T.t
end;
