(* bug180.sml *)

signature S1 =
sig
  type t
end;

signature S2 =
sig
  structure A: S1
  structure B: S1
  sharing type A.t = B.t
end;

structure S: S2 =
struct
  structure A = struct type t = int end
  structure B = struct type t = bool end
end;
