(* Copyright 1997 Bell Laboratories *)

signature CODEGENERATOR = 
sig
  val architecture : string
  val flintcomp : CompBasic.flint * CompBasic.compInfo ->
      CompBasic.csegments * CompBasic.flint option
end (* CODEGENERATOR *)



(*
 * $Log$
 *)
