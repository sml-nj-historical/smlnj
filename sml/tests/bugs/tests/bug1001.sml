(* bug1001.sml *)

structure S : sig val e : exn end = struct exception e end;
