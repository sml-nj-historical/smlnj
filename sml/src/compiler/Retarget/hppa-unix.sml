structure HppaUnixCMB =
    CMBFun (structure TargetCompiler = HppaVisComp
	    val version = "batch (target: hppa-unix)"
	    val targetosn = "unix")
