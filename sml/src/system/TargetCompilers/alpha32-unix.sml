(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure Alpha32UnixCMB =
    BootstrapCompileFn (structure MachDepVC = Alpha32VisComp
			val os = SMLofNJ.SysInfo.UNIX)
