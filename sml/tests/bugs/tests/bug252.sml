(* bug252.sml *)
(* inclusion of signatures with shared types (broken in 0.59) *)

signature SIG1 = sig  type ty = int  end;

signature SIG2 = sig  include SIG1  end;
