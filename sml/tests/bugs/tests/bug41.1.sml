(* bug41.1.sml *)

signature AS = sig val x: int end;

structure A : AS = struct val x = 3 end;

signature BS =
sig
  structure A : AS
end;

structure B : BS =
struct
  open A
end;

