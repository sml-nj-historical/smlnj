(* bug1037.1.sml *)

signature S =
sig
  type s = int
  type 'a t = bool * 'a
  sharing type t = s
end;
