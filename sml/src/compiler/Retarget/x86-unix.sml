structure X86UnixCMB =
    CMBFun (structure TargetMachDepVC = X86VisComp
	    val version = "batch (target: x86-unix)"
	    val targetosn = "unix")
