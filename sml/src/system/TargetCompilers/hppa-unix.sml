(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure HppaUnixCMB =
    BootstrapCompileFn (structure MachDepVC = HppaVisComp
			val os = SMLofNJ.SysInfo.UNIX)
