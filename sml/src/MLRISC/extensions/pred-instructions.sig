(*
 * This signature matches an instruction set that provides full predication
 *)

signature PREDICATED_INSTRUCTIONS =
sig
   include INSTRUCTIONS
   
   type predicate  (* basically says implement it however you want to *)

end

(*
 * $Log: pred-instructions.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:48:27  george
 *   Version 110.10
 *
 *)
