(* bug1144.sml *)

structure B = struct structure A = struct fun f () = () end end;
structure C = struct structure B = B end;
open C;
fun t () = B.A.f;
