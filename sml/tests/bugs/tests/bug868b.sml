(* bug868b.sml *)
(* 868. "Compiler bug: distributeT: dist_abv" caused by type abbrev in sign *)

signature foo = 
sig
  structure A: sig type elmt = int end
end;
