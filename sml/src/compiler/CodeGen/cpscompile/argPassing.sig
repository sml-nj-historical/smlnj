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

