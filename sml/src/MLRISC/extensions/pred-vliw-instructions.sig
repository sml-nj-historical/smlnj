(*
 *  This matches a VLIW instruction set with predication
 *)

signature PREDICATED_VLIW_INSTRUCTIONS =
sig

   include VLIW_INSTRUCTIONS
   type predicate

end

(*
 * $Log: pred-vliw-instructions.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:48:27  george
 *   Version 110.10
 *
 *)
