structure MipsBigUnixCMB =
    CMBFun (structure TargetCompiler = MipsBigVisComp
	    val version = "batch (target: mipseb-unix)"
	    val targetosn = "unix")
