(* bug1254.sml *)

structure S : sig type t end =
struct
  datatype t = T
end;

datatype u = datatype S.t;  (* this should fail: rule (80) *)

val x = T;
