signature STATIC_BRANCH_PREDICTION = 
sig

   structure IR : MLRISC_IR

   val profile : { branchProb     : IR.CFG.block -> int,
                   loopMultiplier : int
                 } -> IR.IR -> unit

end

(*
 * $Log: static-branch-prediction.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:46:54  george
 *  Version 110.10
 *
 *)
