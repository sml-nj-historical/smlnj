(*
 * VLIW instructions involve functional unit assignments
 *)

signature VLIW_INSTRUCTIONS =
sig

   include INSTRUCTIONS
   structure FU : FUNITS       (* functional unit assignment *)
   structure X  : CROSSPATHS   (* for clustered architectures *)

end

(*
 * $Log: vliw-instructions.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:48:27  george
 *   Version 110.10
 *
 *)
