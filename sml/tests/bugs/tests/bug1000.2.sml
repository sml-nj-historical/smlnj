(* bug1000.2.sml *)

(* this is perhaps similar to appel's example in 863 *)
structure U = struct datatype u = U end
structure T = struct datatype t = T of U.u end
val l = U.U
val l = T.T U.U;


