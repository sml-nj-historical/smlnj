structure Alpha32XUnixCMB =
    CMBFun (structure TargetMachDepVC = Alpha32XVisComp
	    val version = "batch (target: alpha32x-unix)"
	    val targetosn = "unix")

structure Alpha32XCompiler = struct
    open Alpha32XVisComp
    open GenericVC
end
