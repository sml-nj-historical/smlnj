(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure Alpha32UnixCMB : CMB =
    BootstrapCompileFn (structure Backend = Alpha32Backend
			val useStream = Backend.Interact.useStream
			val os = SMLofNJ.SysInfo.UNIX
			val load_plugin = CM0.load_plugin)
