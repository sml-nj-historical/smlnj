(* bug590.sml *)
(* checking some tyvars now illegal; were handled wrong before *)

val x : '_1abcd = 3;

(* There should be an syntax error in the above since that is no longer
a legal type variable name. *)
