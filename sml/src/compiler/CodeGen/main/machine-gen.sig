(* COPYRIGHT (c) 1999 Lucent Technologies, Bell Labs *)
(* Generation of machine code from a list of CPS functions *)

signature MACHINE_GEN = sig			
  include MACHINE
  structure MLTreeComp : MLTREECOMP
		where CFG = CFG
		  and I = CFG.I
  structure InvokeGC   : INVOKE_GC
		where CFG=MLTreeComp.CFG
		  and T = MLTreeComp.T
  structure Shuffle    : SHUFFLE 
		where I = MLTreeComp.I
  structure MachSpec   : MACH_SPEC

  val codegen : CPS.function list * (CPS.lvar -> (int * int))
                * ErrorMsg.complainer -> unit
end (* MACHINE_GEN *)

