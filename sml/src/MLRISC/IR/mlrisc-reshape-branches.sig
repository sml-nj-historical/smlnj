(*
 * Restructure the branches according to the branch frequencies 
 * in the program.  Try to eliminate the number of branches within a loop.
 *)
signature RESHAPE_BRANCHES =
sig

   structure IR : MLRISC_IR

   val reshapeBranches : IR.IR -> unit

end

(*
 * $Log: mlrisc-reshape-branches.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:46:54  george
 *  Version 110.10
 *
 *)
