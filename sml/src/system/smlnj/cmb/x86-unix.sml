(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure X86UnixCMB =
    BootstrapCompileFn (structure MachDepVC = X86VisComp
			val useStream = Compiler.Interact.useStream
			val os = SMLofNJ.SysInfo.UNIX
			val load_plugin = CM0.load_plugin)


