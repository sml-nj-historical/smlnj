structure MipsBigUnixCMB =
    CMBFun (structure TargetMachDepVC = MipsBigVisComp
	    val version = "batch (target: mipseb-unix)"
	    val targetosn = "unix")
