(* bug187.sml *)
(* parsing clausal definitions with infix functions *)

infix xxx;
fun (a xxx b)   = b; (* Should compile *)
fun (a xxx b) c = c; (* Should compile *)
fun a xxx b y = y; (* Shouldn't compile *)
