(* bug1392.2.sml *)

structure A =
struct

  datatype A = a | b

  fun f a = true
    | f a = false
    | f b = true
end;
