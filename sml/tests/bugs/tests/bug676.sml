(* bug676.sml *)
(* 676. Out of order record field evaluation. *)

val r = ref 0;
fun inc r = r := !r+1
fun g() = (inc r; !r);
{last=g(),first=g()};
