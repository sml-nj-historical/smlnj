(* bug1434.1.sml *)

structure A =
struct
  open B
  val (x::y) = [1,2]
end;
