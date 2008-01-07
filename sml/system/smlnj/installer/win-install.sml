(*
 * Win32-specific invocation of LibInstall.proc (see libinstall.sml).
 *
 * (C) 2003 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure WinInstall : sig end = struct

    structure I = GenericInstall

    fun proc () =
	let val home = valOf (OS.Process.getEnv "SMLNJ_HOME")
	in I.proc { smlnjroot = home,
		    installdir = home,
		    buildcmd = "build.bat",
		    unpack = NONE,
		    instcmd = fn target =>
				 Copy.copy { from = concat [home, "\\config\\",
							    target, ".bat"],
					     to = concat [home, "\\bin\\",
							  target, ".bat"] } }
	end

    (* doit right away *)
    val _ = proc ()
end
