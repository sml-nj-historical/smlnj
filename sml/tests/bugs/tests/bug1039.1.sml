(* bug1039.1.sml *)

structure C =
struct
  structure Elem = struct end
end;

signature MV =
sig
  structure Elem : sig end
end;

signature MA =
sig
  structure Elem : sig end
  structure Vector : MV
  sharing Vector.Elem = Elem
end;

structure s : MA = C;
