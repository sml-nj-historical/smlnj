(* argPassing.sml --- parameter passing convention for standard
 *		or known functions.
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)
signature ARG_PASSING = sig
  structure T : MLTREE
  val standard : CPS.cty * CPS.cty list -> T.mlrisc list  
  val known : CPS.cty list -> T.mlrisc list
  val fixed : CPS.cty list -> T.mlrisc list
end

(*
 * $Log: argPassing.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:54  george
 * Version 110.5
 *
 *)
