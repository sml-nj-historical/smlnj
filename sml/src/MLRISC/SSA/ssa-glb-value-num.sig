signature SSA_GLOBAL_VALUE_NUMBERING =
sig

   structure SSA : SSA

   val computeValueNumbers : SSA.ssa -> int Array.array
   val top : int

end

(*
 * $Log: ssa-glb-value-num.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:47:06  george
 *  Version 110.10
 *
 *)
