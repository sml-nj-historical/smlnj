(*
 * Unix-specific invocation of LibInstall.proc (see libinstall.sml).
 *
 * Copyright (c) 2003 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure UnixLibInstall : sig end = struct

    fun proc () = let
	val home = valOf (OS.Process.getEnv "ROOT")
	val installdir = getOpt (OS.Process.getEnv "INSTALLDIR", home)
	val configdir = getOpt (OS.Process.getEnv "CONFIGDIR",
				OS.Path.concat (home, "config"))
	val unpack = OS.Path.concat (configdir, "unpack")
	val bindir = getOpt (OS.Process.getEnv "BINDIR",
			     OS.Path.concat (installdir, "bin"))
	fun bincmd cmd = OS.Path.concat (bindir, cmd)
	val runsml = ".run-sml"		(* don't prepend bindir! *)
    in
	LibInstall.proc { smlnjroot = home,
			  installdir = installdir,
			  buildcmd = "CM_LOCAL_PATHCONFIG=/dev/null ./build",
			  unpackcmd = SOME unpack,
			  instcmd = fn target => let
					   val new = bincmd target
				       in
					   if OS.FileSys.access (new, []) then
					       ()
					   else
					       Posix.FileSys.symlink
						   { old = runsml, new = new }
				       end }
    end

    val _ = proc ()
end
