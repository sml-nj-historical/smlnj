(*
 * This is a stub providing "slave" functionality for CMB.
 * (We use dynamic linking technology to avoid loading target-compilers.cm
 *  on the slave side unless it is really needed.)
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure CMBSlave = struct
    local
	val lib = "target-compilers.cm"
	val loaded = ref false
	val table =
	    foldl StringMap.insert' StringMap.empty
	    [("alpha32-unix", "Alpha32UnixCMB.make"),
	     ("hppa-unix", "HppaUnixCMB.make"),
	     ("ppc-macos", "PPCMacOSCMB.make"),
	     ("ppc-unix", "PPCUnixCMB.make"),
	     ("sparc-unix", "SparcUnixCMB.make"),
	     ("x86-unix", "X86UnixCMB.make"),
	     ("x86-win32", "X86Win32CMB.make")]
    in
	(* "load" is supposed to be CM.autoload and "touch" should be
	 * (Compiler.Interact.useStream o TextIO.openString) *)
	fun slave { load, touch } arch s =
	    case StringMap.find (table, arch) of
		NONE => NONE
	      | SOME cmd =>
		    (if !loaded then ()
		     else if load lib then loaded := true
		     else raise Fail (concat ["dynamic linkage for CMB slave ",
					      arch, " failed"]);
		     touch cmd;
		     CMBSlaveHook.slave arch s)
    end
end
