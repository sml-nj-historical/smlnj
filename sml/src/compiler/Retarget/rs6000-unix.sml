structure RS6000UnixCMB =
    CMBFun (structure TargetCompiler = RS6000VisComp
	    val version = "batch (target: rs6000-unix)"
	    val targetosn = "unix")
