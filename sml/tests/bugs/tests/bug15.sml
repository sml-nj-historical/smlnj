(* bug15.sml *)

signature SIG = sig type t end;
signature STWO = sig structure X:SIG and Y:SIG sharing X.t=Y.t end;
