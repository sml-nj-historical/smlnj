structure SparcUnixCMB =
    CMBFun (structure TargetCompiler = SparcVisComp
	    val version = "batch (target: sparc-unix)"
	    val targetosn = "unix")
