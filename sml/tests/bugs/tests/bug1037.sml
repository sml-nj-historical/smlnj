(* bug1037.sml *)

signature S =
sig
  type s = int
  type t = bool
  sharing type t = s
end;
