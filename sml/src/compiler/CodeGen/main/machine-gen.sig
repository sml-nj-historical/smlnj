(* COPYRIGHT (c) 1999 Lucent Technologies, Bell Labs *)
(* Generation of machine code from a list of CPS functions *)

signature MACHINE_GEN = sig			
  include MACHINE
  structure MLTreeComp : MLTREECOMP
  structure InvokeGC   : INVOKE_GC
  structure MachSpec : MACH_SPEC
     sharing InvokeGC.T   = MLTreeComp.T
     sharing MLTreeComp.I = F.I

  val codegen : CPS.function list * (CPS.lvar -> (int * int))
                * ErrorMsg.complainer -> unit
end (* MACHINE_GEN *)

