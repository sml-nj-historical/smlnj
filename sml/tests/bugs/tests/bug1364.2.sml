(* bug1364.2.sml *)

structure S :
sig
   datatype t = T
end =
struct
   datatype u = T
   datatype t = datatype u
end;
