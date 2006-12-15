(* Copyright 1999, Lucent Technologies, Bell Labs *)

(* Generation of machine code from the flint intermediate form *)
signature CODEGENERATOR = 
sig
  structure Machine : MACHINE_GEN
  val architecture : string
  val abi_variant : string option (* to distinguish between, e.g., various
				 * intel-based unices, etc.*)
  (* the int option gets passed to lambda-split phases (if any) *)
  val flintcomp : FLINT.prog * Absyn.dec CompInfo.compInfo * int option ->
      (CodeObj.csegments * FLINT.prog option)
end (* CODEGENERATOR *)
