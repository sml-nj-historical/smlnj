(* bug83.sml *)
(* unexpected parsing of erroneous datatype declaration *)

datatype complex = Complex (real,real);
