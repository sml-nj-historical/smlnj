(* bug991.sml *)

signature S=
sig
  type 'a t
  type s
end;

signature T=
sig
  type u = int
end;

signature U =
sig
  structure A: S
  structure B: T
  type v=B.u A.t
  sharing type A.s=v
end;

