(* bug916.sml *)
(* 916. Compiler bug: Misc.checkbound from type abbrev in sig *)

signature FOO = 
sig
  type 'a t = 'a list
end;
