(* PseudoOpType.sig -- signature to expose the pseudo-op constructors
 * 
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

signature SMLNJ_PSEUDO_OPS = sig
  datatype smlnj_pseudo_op = 
      JUMPTABLE of {base:Label.label, targets:Label.label list}
    | FILENAME of string

  include CLIENT_PSEUDO_OPS where type pseudo_op = smlnj_pseudo_op
end




