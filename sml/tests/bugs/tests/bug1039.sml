(* bug1039.sml *)

structure C =
struct
  type elem = int
end;

signature MV =
sig
  eqtype  elem
end;

signature MA =
sig
  eqtype  elem
  structure Vector : MV
  sharing type Vector.elem = elem
end;

structure s : MA = C;
