(* bug1152.2.sml *)

structure A :> 
  sig
    eqtype t
    structure B: sig type t end
    sharing type t = B.t
  end =
struct
  type t = int
  structure B = struct type t = t end
end;

fun f(x: A.t) = x = x;  (* should work? *)
