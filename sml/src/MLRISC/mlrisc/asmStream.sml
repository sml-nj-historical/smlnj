(* asmStream.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(* AsmStream - this structure is available to all codegenerators.
 *             Typically asmOutStream is rebound to a file.
 *)

structure AsmStream = 
    struct
	val asmOutStream = ref TextIO.stdOut
    end



(*
 * $Log: asmStream.sml,v $
 * Revision 1.1.1.1  1997/04/19 18:14:19  george
 *   Version 109.27
 *
 *)
