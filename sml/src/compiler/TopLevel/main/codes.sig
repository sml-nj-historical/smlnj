(* Copyright 1997 Bell Laboratories *)

signature CODEGENERATOR = 
sig
  val architecture : string
  val flintcomp : CompBasic.flint * CompBasic.compInfo -> CompBasic.csegments
end (* CODEGENERATOR *)



(*
 * $Log: codes.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:15  george
 * Version 110.5
 *
 *)
