(* bug613.sml *)
(* 613. Compiler bug message on occurence of typevar in signature *)

signature BuggySignature = 
  sig exception IllegalException of '_a ref end;
