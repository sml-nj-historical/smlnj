(* instruction.sig --- target machine instructions 
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

(* basically says: 
 * represent instructions any which way you want 
 *)
signature INSTRUCTIONS = sig
    structure C : CELLS
    structure Constant : CONSTANT

    type ea
    type operand    
    type instruction
end


(*
 * $Log: instructions.sig,v $
 * Revision 1.2  1998/02/16 13:58:19  george
 *   A register allocated temp is now associated with parallel COPYs
 *   instead of a dedicated register. The temp is used to break cycles.
 *
 * Revision 1.1.1.1  1997/04/19 18:14:20  george
 *   Version 109.27
 *
 *)
