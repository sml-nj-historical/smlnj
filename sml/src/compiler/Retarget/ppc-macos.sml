(* ppc-macos.sml
 *
 * COPYRIGHT (c) 1999 Bell Labs, Lucent Technologies.
 *)

structure PPCMacOSCMB =
    CMBFun (structure TargetMachDepVC = PPCVisComp
	    val version = "batch (target: ppc-macos)"
	    val targetosn = "macos")

structure PPCCompiler = struct
    open PPCVisComp
    open GenericVC
end
