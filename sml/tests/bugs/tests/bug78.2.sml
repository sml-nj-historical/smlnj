(* bug78.2.sml *)
(* bad signature allowed *)

signature FRED =
sig
  type Fred
  val x: 'a Fred
end;
