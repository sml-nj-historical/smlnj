(* bug1152.1.sml *)

structure A :> 
  sig
    eqtype t
  end =
struct
  type t = int
end;

fun f(x: A.t) = x = x;
