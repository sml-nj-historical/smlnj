(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure PPCUnixCMB =
    BootstrapCompileFn (structure MachDepVC = PPCVisComp
			val os = SMLofNJ.SysInfo.UNIX)
