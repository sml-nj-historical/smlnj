(* bug842.sml *)
(* 842. sharing constraints in functor sig *)


funsig S1 (type t=string)= sig type t=int end;
