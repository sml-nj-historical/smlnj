(* 173.sml *)
(* Should this produce an error?  Structure decl nested in a value decl? *)

structure S =
struct
  datatype d = A | B of int
  val x : d = B 3
end;

signature ONLY_D =
sig
  datatype d = A | B of int
end;

structure T =
struct
  local
    structure Export : ONLY_D = S
  in
    open Export
  end
end;
