(* bug99.sml *)

signature A = sig end;
signature B = sig include A end;
