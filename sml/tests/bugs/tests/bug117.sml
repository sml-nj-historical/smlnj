(* bug117.sml *)

(***************************************************************************)
(* This is illegal in version 3 of the ML standard                         *)
(* s may only be elaborated to a non-equality type (+ extra bits)          *)
(* t may only be elaborated to an equality type (for consistency with its  *)
(* constructor environment)                                                *)
(* Hence s and t can't share                                               *)
(***************************************************************************)

signature BADSIG =
sig
  datatype s = Dummy of bool -> bool
  datatype t = Dummy of int
  sharing type s = t;
end;

