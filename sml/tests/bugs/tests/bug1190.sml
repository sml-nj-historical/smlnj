(* bug1190.sml *)

structure STR :> sig exception E end =
struct
  val E = Overflow
end;
