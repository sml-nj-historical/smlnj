(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure X86Win32CMB =
    BootstrapCompileFn (structure MachDepVC = X86VisComp
			val os = SMLofNJ.SysInfo.WIN32
			val load_plugin = CM.load_plugin)
