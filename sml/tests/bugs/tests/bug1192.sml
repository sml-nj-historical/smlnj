(* bug1192.sml *)

datatype d = D;
let datatype t = datatype d in () end;
