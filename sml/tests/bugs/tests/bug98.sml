(* bug98.sml *)
(* compiler bug in defineEqTycon *)

datatype constant_type = CONSTANT;

datatype composed_type = Constructor of int * CONSTANT;
