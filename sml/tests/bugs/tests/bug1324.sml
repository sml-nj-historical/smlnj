(* bug1324.sml *)

signature K =
sig
  type s
  include sig eqtype t end where type t = s
end;
