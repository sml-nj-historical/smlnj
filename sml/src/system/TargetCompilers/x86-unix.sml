(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure X86UnixCMB =
    BootstrapCompileFn (structure MachDepVC = X86VisComp
			val os = SMLofNJ.SysInfo.UNIX)


