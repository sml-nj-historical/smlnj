(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure HppaUnixCMB =
    BootstrapCompileFn (structure MachDepVC = HppaVisComp
			val os = SMLofNJ.SysInfo.UNIX
			val load_plugin = CM.load_plugin)
