(* (C) 1999 Lucent Technologies, Bell Laboratories *)

structure HppaUnixCMB : CMB =
    BootstrapCompileFn (structure Backend = HppaBackend
			val useStream = Backend.Interact.useStream
			val os = SMLofNJ.SysInfo.UNIX
			val load_plugin = CM0.load_plugin)
