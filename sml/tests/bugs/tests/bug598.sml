(* bug598.sml *)
(* Compiler bug after incorrect datatype/withtype declaration. *)

datatype 'a t = A of u
  withtype 'a u = 'a list;

(* There should not be a compiler bug: applyTyfun: not enough arguments
   occuring here. *)
