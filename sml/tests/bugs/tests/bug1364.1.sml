(* bug1364.1.sml *)

structure S :
sig
   datatype t = T
end =
struct
   datatype u = T
   type t = u
end;
