(* bug599.sml *)

signature SA =
sig
  type t
end;

signature SB = 
sig
  type 'a s
end;

signature SC =
sig
  structure A: SA
  structure B: SB
  sharing type A.t = B.s
end;
