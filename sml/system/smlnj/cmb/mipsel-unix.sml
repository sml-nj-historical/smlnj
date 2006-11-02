(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure MipsLittleUnixCMB =
    BootstrapCompileFn (structure MachDepVC = MipsLittleVisComp
			val os = SMLofNJ.SysInfo.UNIX)
