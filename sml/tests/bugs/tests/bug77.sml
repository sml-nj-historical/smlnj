(* bug77.sml *)
(* unparenthesized infix expressions in fun lhs *)

infix 4 %;
infix 3 %%;

datatype foo = op % of int * int;
fun a % b %% c % d = 0;
