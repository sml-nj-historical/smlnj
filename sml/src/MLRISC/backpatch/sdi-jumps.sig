(* sdi-jumps.sig --- specification of target information to resolve jumps. 
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature SDI_JUMPS = sig
  structure I : INSTRUCTIONS
  structure C : CELLS
    sharing I.C = C

  val branchDelayedArch : bool

  val isSdi : I.instruction -> bool
  val minSize : I.instruction -> int
  val maxSize : I.instruction -> int
      (* minSize and maxSize are not restricted to SDIs but 
       * instructions that may require NOPs after them, etc. 
       *)

  val sdiSize : I.instruction * int Intmap.intmap
			      * (Label.label -> int) * int -> int
      (* sdiSize(instr, regmaps, labMap, loc) -- return the size of
       * instr at location loc, assuming an assignment of labels
       * given by labMap.
       *)

  val expand : I.instruction * int * int -> I.instruction list
      (* expand(instr,size,loc) - expands sdi instruction instr,
       *  into size bytes at postion loc.
       *)

end


(*
 * $Log: sdi-jumps.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:47:14  george
 *   Version 110.10
 *
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
