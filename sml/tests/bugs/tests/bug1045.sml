(* bug1045.sml *)

signature S =
sig
  structure A: sig val x:int end
end;

structure X: S =
struct
  structure A = struct end
end;
