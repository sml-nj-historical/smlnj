(* bug1042.2.sml *)

structure A = struct end;

structure S =
struct
  structure B : sig type t end = A
  open B
end;
