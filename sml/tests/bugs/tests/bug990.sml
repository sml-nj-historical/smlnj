(* bug990.sml *)
(* Error: Compiler bug: access_type" while elaborating a signature *)

signature U =
sig
  structure A:sig type 'a t type s end
  structure B:sig type u = int end
  type v=B.u A.t
  sharing type A.s=v
end;

