(*
 * Signature for an SSA optimization phase
 *)
signature SSA_OPTIMIZATION =
sig

   structure SSA : SSA 

   val optimize : SSA.ssa -> SSA.ssa

end

(*
 * $Log: ssa-optimization.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:47:06  george
 *  Version 110.10
 *
 *)
