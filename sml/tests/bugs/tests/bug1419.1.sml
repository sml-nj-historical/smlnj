(* bug1419.1.sml *)

signature S = sig type s type t end;
signature S1 =
sig
  type s
  structure A : S where type t = s
end;
