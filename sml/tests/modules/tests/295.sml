(* 295.sml *)
(* rigid type in sharing spec *)

signature SIG0 =
sig
  type s
end;

signature SIG1 =
sig
  structure S : SIG0
  type t = S.s
end;

signature SIG2 =
sig
  type u
  structure A : SIG1
  sharing type u = A.t  (* should fail because A.t rigid *)
end;

functor F(X: SIG2) = struct end;
