(* bug1520.2.sml *)

signature S =
sig
  type ('a,'b) t = 'a
end;

structure A : S =
struct
  type ('a,'b) t = 'b
end;
