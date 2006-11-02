(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure RS6000UnixCMB =
    BootstrapCompileFn (structure MachDepVC = RS6000VisComp
			val os = SMLofNJ.SysInfo.UNIX)
