(* bug80.sml *)
(* simultaneous type declarations *)

type type2 = int
and  type3 = type2 * string;
