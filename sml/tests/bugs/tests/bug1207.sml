(* bug1207.sml *)

signature SIG =
sig 
  datatype d = c include sig type t end where type t = d 
end;
