(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure PPCUnixCMB =
    BootstrapCompileFn (structure MachDepVC = PPCVisComp
			val os = SMLofNJ.SysInfo.UNIX
			val load_plugin = CM0.load_plugin)
