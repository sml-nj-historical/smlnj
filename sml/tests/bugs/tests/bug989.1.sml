(* bug989.1.sml *)
(* 989. Compiler bug: Instantiate:explore_tclass.5 *)

signature S =
sig
  type T
  sharing type T = unit 
end;
