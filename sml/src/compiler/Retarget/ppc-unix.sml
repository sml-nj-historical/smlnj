structure PPCUnixCMB =
    CMBFun (structure TargetMachDepVC = PPCVisComp
	    val version = "batch (target: ppc-unix)"
	    val targetosn = "unix")

structure PPCCompiler = struct
    open PPCVisComp
    open GenericVC
end