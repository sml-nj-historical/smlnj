(* bug1578.1.sml *)

type u = int

fun f() =
   let
      datatype t = datatype u
   in ()
   end
