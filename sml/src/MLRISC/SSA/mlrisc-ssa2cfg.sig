signature SSA2CFG =
sig

   structure SSA : SSA
   structure CFG : CONTROL_FLOW_GRAPH
     sharing SSA.CFG = CFG

   val buildCFG : SSA.ssa -> CFG.cfg 

end

(*
 * $Log: mlrisc-ssa2cfg.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:47:05  george
 *  Version 110.10
 *
 *)
