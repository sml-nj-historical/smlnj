(* bug1369.1.sml *)

signature SIG0 =
sig
  structure B: sig structure A: sig end end
end;

signature SIG1 =
sig
  structure C : SIG0
  structure D : SIG0
end;

signature SIG2 =
sig
  structure C : SIG0
  structure D : SIG0
  sharing C = D
end;

functor F (X : SIG2) = struct end;

functor H (Y : SIG1) =
struct
  structure E = F(Y)
end;


(* Puzzle: in compStr in SigMatch, when matching instantiated
Y to SIG2, C and D have same signatures, but substructures C.B and
D.B apparently have unequal signatures (according to MU.eqSign).
Did Instantiate change the common signature (of B in SIG0)?
*)
