(* bug1351.sml *)

abstype A = B with datatype A = datatype A end;
