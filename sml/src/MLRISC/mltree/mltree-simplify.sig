(*
 * Performs simple local optimizations.
 * Constant folding, algebraic simplication and some dead code elimination.
 *)
signature MLTREE_SIMPLIFIER =
sig

   structure T : MLTREE

   type simplifier = T.rewriter
   val simplify : { addressWidth : int } -> simplifier
   
end
