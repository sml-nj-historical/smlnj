(* Copyright 1999, Lucent Technologies, Bell Labs *)

(* Generation of machine code from the flint intermediate form *)
signature CODEGENERATOR = 
sig
  structure Machine : MACHINE_GEN
  val architecture : string
  val flintcomp : CompBasic.flint * CompBasic.compInfo ->
      (CompBasic.csegments * CompBasic.flint option)
end (* CODEGENERATOR *)
