(* bug637.sml *)
(* Compiler bug printing signatures with polymorphic exceptions *)

signature SEQUENCE =
sig 
  exception no_next of 'a
end;
