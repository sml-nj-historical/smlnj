(* bug38.sml *)

signature FOO1 =
sig
  structure S: sig type S end
  structure T: sig type T end
  sharing S = T
end;
