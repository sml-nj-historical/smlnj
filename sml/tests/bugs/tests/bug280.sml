(* bug280.sml *)

signature S1 = sig infix ## end;

signature S2 = sig include S1 end;

