(* bug1242.sml *)

structure A =
struct
  datatype at = A | B | C of int
end;

local
  open A Foo
in
  fun toString A = "A"
    | toString B = "B"
    | toString (C i) = "C " ^ Int.toString i
end;
