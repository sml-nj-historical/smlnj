(* PseudoOpType.sig -- signature to expose the pseudo-op constructors
 * 
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

signature SMLNJ_PSEUDO_OP_TYPE = sig
  datatype pseudo_op = JUMPTABLE of {base:Label.label, targets:Label.label list}
end

(*
 * $Log: pseudoOpType.sig,v $
 * Revision 1.3  1998/11/18 03:53:08  jhr
 *  New array representations.
 *
 * Revision 1.2  1998/10/28 18:20:39  jhr
 *   Removed code generator support for STRING/REAL constants.
 *
 * Revision 1.1.1.1  1998/04/08 18:39:54  george
 * Version 110.5
 *
 *)
