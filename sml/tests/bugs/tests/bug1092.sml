(* bug1092.sml *)

datatype m = M of ifc option
and ifc = IFC;

val x = SOME IFC;  (* must be SOME *)

val y = M x;
