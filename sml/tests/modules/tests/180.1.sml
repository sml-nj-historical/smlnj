(* 180.1.sml *)
(* syntax of multiple sharing equations in one spec *)

signature S1 =
sig
  type t
  type s
  type v
  sharing type t = s
      and type s = v
end;
