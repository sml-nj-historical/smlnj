structure SparcUnixCMB =
    CMBFun (structure TargetMachDepVC = SparcVisComp
	    val version = "batch (target: sparc-unix)"
	    val targetosn = "unix")

structure SparcCompiler = struct
    open SparcVisComp
    open GenericVC
end
