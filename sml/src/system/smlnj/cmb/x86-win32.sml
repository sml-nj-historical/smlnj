(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure X86Win32CMB : CMB =
    BootstrapCompileFn (structure MachDepVC = X86VisComp
			val useStream = Compiler.Interact.useStream
			val os = SMLofNJ.SysInfo.WIN32
			val load_plugin = CM0.load_plugin)
