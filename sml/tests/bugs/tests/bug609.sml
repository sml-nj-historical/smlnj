(* bug609.sml *)
(* include syntax *)

signature SIG1 = sig val x : int end;
signature SIG2 = sig val y : int end;
    
signature SIG3 = sig include SIG1 SIG2 end;
