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
 * $Log$
 *)
