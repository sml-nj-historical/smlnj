(* bug3.sml *)

structure S = struct datatype t = A val x = A end;
open S;
structure S = struct end;
x;
