structure MipsLittleUnixCMB =
    CMBFun (structure TargetCompiler = MipsLittleVisComp
	    val version = "batch (target: mipsel-unix)"
	    val targetosn = "unix")
