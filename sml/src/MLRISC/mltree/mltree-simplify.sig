(*
 * Performs simple local optimizations.
 * Constant folding, algebraic simplication and some dead code elimination.
 *)
signature MLTREE_SIMPLIFIER =
sig

   structure T : MLTREE

   val simplify      : T.stm -> T.stm
   val simplifyRexp  : T.rexp -> T.rexp
   val simplifyFexp  : T.fexp -> T.fexp
   val simplifyCCexp : T.ccexp -> T.ccexp

end
