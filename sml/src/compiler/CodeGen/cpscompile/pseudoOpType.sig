(* PseudoOpType.sig -- signature to expose the pseudo-op constructors
 * 
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

signature SMLNJ_PSEUDO_OP_TYPE = sig
  datatype pseudo_op = 
      MARK
    | REALCONST of Label.label * string
    | STRINGCONST of Label.label * int * string
    | JUMPTABLE of {base:Label.label, targets:Label.label list}
end

(*
 * $Log: pseudoOpType.sig,v $
 * Revision 1.1.1.1  1997/01/14 01:38:34  george
 *   Version 109.24
 *
 *)
