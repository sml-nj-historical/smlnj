(* ppc-macos.sml
 *
 * COPYRIGHT (c) 1999 Bell Labs, Lucent Technologies.
 *)

structure PPCMacOSCMB : CMB =
    BootstrapCompileFn (structure MachDepVC = PPCVisComp
			val useStream = Compiler.Interact.useStream
			val os = SMLofNJ.SysInfo.MACOS
			val load_plugin = CM0.load_plugin)
