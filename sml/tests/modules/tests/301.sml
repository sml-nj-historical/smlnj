(* 301.sml *)
(* related to bug 1445 *)

signature S1 =
sig
  type t
end;

signature S2 =
sig
  structure U : S1
  datatype r = Kr of U.t
end;

signature S3 =
sig
  structure T : S2
end;

signature S4 =
sig
  type s
  datatype t = Kt of s
end;

(* S5 elaborates ok - no cycles *)
signature S5 =
sig
  structure C : S3
  structure D : S3 where T = C.T
  structure E : S4
  sharing type C.T.U.t = E.t
end;

functor F(X: S5) =
struct
  val x = X.D.T.Kr
end;
