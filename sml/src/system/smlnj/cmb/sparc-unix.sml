(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure SparcUnixCMB =
    BootstrapCompileFn (structure MachDepVC = SparcVisComp
			val os = SMLofNJ.SysInfo.UNIX
			val load_plugin = CM0.load_plugin)
