(* ppc-macos.sml
 *
 * COPYRIGHT (c) 1999 Bell Labs, Lucent Technologies.
 *)

structure PPCMacOSCMB =
    BootstrapCompileFn (structure MachDepVC = PPCVisComp
			val os = SMLofNJ.SysInfo.MACOS
			val load_plugin = CM0.load_plugin)
