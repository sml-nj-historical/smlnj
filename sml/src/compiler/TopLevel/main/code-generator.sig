(* Copyright 1999, Lucent Technologies, Bell Labs *)

(* Generation of machine code from the flint intermediate form *)
signature CODEGENERATOR = 
sig
  structure Machine : MACHINE_GEN
  val architecture : string
  (* the int option gets passed to lambda-split phases (if any) *)
  val flintcomp : FLINT.prog * CompInfo.compInfo * int option ->
      (CodeObj.csegments * FLINT.prog option)
end (* CODEGENERATOR *)
