(*
 * Win32-specific invocation of LibInstall.proc (see libinstall.sml).
 *
 * (C) 2003 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure WinLibInstall : sig end = struct

    fun proc () = let
	val home = valOf (OS.Process.getEnv "SMLNJ_HOME")

	fun copy { from, to } = let
	    val ins = TextIO.openIn from
	    val outs = TextIO.openOut to
	    fun loop NONE = (TextIO.closeIn ins; TextIO.closeOut outs)
	      | loop (SOME l) = (TextIO.output (outs, l); next ())
	    and next () = loop (TextIO.inputLine ins)
	in
	    next ()
	end
    in
	LibInstall.proc
	    { smlnjroot = home,
	      installdir = home,
	      buildcmd = "build.bat",
	      unpackcmd = NONE,
	      instcmd = fn target =>
			   copy { from = concat [home, "\\config\\",
						 target, ".bat"],
				  to = concat [home, "\\bin\\",
					       target, ".bat"] } }
    end

    (* doit right away *)
    val _ = proc ()
end
