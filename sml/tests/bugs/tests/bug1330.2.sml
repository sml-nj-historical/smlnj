(* bug1330.2.sml *)

signature S =
sig
  type t
  type s = t
  sharing type t = s
end;

