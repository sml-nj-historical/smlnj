(* bug837.sml *)

exception NoInverse;
val vinv = op /;

vinv(1.0,0.0) handle _ => raise NoInverse;
