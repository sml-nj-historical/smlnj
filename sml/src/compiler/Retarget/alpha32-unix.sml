structure Alpha32UnixCMB =
    CMBFun (structure TargetMachDepVC = Alpha32VisComp
	    val version = "batch (target: alpha32-unix)"
	    val targetosn = "unix")

structure Alpha32Compiler = struct
    open Alpha32VisComp
    open GenericVC
end
