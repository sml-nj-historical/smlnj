structure MipsLittleUnixCMB =
    CMBFun (structure TargetMachDepVC = MipsLittleVisComp
	    val version = "batch (target: mipsel-unix)"
	    val targetosn = "unix")

structure MipsLittleCompiler = struct
    open MipsLittleVisComp
    open GenericVC
end
