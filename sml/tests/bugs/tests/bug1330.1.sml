(* bug1330.1.sml *)

signature S =
sig
  type t
  type s = t
end where type s = int;

