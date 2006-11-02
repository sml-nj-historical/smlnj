(* argPassing.sml --- parameter passing convention for standard
 *		or known functions.
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)
signature ARG_PASSING = sig
  structure T : MLTREE
  val standard : {fnTy: CPS.cty, vfp:bool, argTys: CPS.cty list} -> T.mlrisc list  
  val fixed : {argTys:CPS.cty list, vfp:bool} -> T.mlrisc list
end
