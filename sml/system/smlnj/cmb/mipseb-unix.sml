(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure MipsBigUnixCMB =
    BootstrapCompileFn (structure MachDepVC = MipsBigVisComp
			val os = SMLofNJ.SysInfo.UNIX)
