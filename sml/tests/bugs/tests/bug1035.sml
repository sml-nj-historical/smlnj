(* bug1035.sml *)

signature S =
sig
  type t = int
end;

structure A: S =
struct
  type t = bool
end;
