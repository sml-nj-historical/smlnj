structure HppaUnixCMB =
    CMBFun (structure TargetMachDepVC = HppaVisComp
	    val version = "batch (target: hppa-unix)"
	    val targetosn = "unix")

structure HppaCompiler = struct
    open HppaVisComp
    open GenericVC
end
