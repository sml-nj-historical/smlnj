(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure SparcUnixCMB : CMB =
    BootstrapCompileFn (structure MachDepVC = SparcVisComp
			val useStream = Compiler.Interact.useStream
			val os = SMLofNJ.SysInfo.UNIX
			val load_plugin = CM0.load_plugin)
