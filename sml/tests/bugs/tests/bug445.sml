(* bug445.sml *)

signature  T = 
sig
 datatype debuglevel = A of TextIO.instream option
end
