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
 * Revision 1.1.1.1  1998/04/08 18:39:54  george
 * Version 110.5
 *
 *)
