(* bug 595 (jhr) *)

(* This lead to an UnboundTable exception  because the function thinning
   the functor body according to the signature used look without handle P.C
   in sigmatch.sml *)

(* status: changed should be put in next version (.87) *)

signature A = sig type t end;

functor f(): A = struct end;

