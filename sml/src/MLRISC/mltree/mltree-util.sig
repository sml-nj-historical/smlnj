(*
 * Basic utility functions for MLTREE
 *
 * -- Allen
 *)

signature MLTREE_UTIL =
sig

  structure Basis : MLTREE_BASIS

  (* Invert the conditional when swapping the two arguments
   * of the comparision.  This is not the negation!
   *)
  val swapCond   : Basis.cond -> Basis.cond

  (* 
   * This is the negation! 
   *)
  val negateCond : Basis.cond -> Basis.cond

   (* hashing functions *)
  val hashCond  : Basis.cond -> word
  val hashFcond : Basis.fcond -> word
  val hashExt   : Basis.ext -> word
  val hashRoundingMode : Basis.rounding_mode -> word

   (* pretty printing *)
  val condToString  : Basis.cond -> string
  val fcondToString : Basis.fcond -> string
  val extToString   : Basis.ext -> string
  val roundingModeToString : Basis.rounding_mode -> string
 
end
