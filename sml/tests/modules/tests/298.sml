(* 298.sml *)
(* related to bug 1445 *)

signature S0 =
sig
  type t
end;

signature S1 =
sig
  structure U : S0
  datatype r = Kr of U.t
end;

signature S2 =
sig
  structure T : S1
end;

signature S3 =
sig
  type s
  datatype t = Kt of s
end;

functor F
  (structure C : S2
   structure D : S2 where T = C.T
   structure E : S3
   sharing type C.T.U.t = E.t) =
struct
  val x = D.T.Kr
end;
