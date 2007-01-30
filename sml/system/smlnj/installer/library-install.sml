(* library-install.sml
 *    Installer routine for additional libraries.
 *
 * (C) 2007 The Fellowship of SML/NJ
 *
 * author: Matthias Blume
 *)

(**** TODO: factor out all common functionality between this and
 *          generic-install.sml *)

structure LibraryInstall : sig end = struct

    structure P = OS.Path
    structure F = OS.FileSys
    structure SI = SMLofNJ.SysInfo

    fun say l = TextIO.output (TextIO.stdErr, concat l)
    fun warn l = say ("WARNING: " :: l)
    fun fail l = (say ("FAILURE: " :: l);
		  OS.Process.exit OS.Process.failure)

    fun pconc [] = ""
      | pconc [p] = p
      | pconc (p :: ps) = P.concat (p, pconc ps)

    fun usage () =
	say ["sml -m $smlnj/lib-install.cm src libdir tgt\n",
	     "\tsrc: .cm-file for library (path name in Unix syntax)\n",
	     "\tlibdir: library directory (path name in native syntax)",
	     "\ttgt: .cm-file for destination (Unix-syntax, ",
	     "relative to libdir)\n"]
	 
    (* figure out who and what we are *)
    val arch = String.map Char.toLower (SMLofNJ.SysInfo.getHostArch ())
    val (isUnix, oskind) = case SI.getOSKind () of
			       SI.UNIX => (true, "unix")
			     | SI.WIN32 => (false, "win32")
			     | _ => fail ["os kind not supported\n"]

    val arch_oskind = concat [arch, "-", oskind]

    fun fexists f = F.access (f, []) handle _ => false
    fun rmfile f = F.remove f handle _ => ()

    (* make a directory (including parent, parent's parent, ...) *)
    fun mkdir "" = ()
      | mkdir d = if fexists d then () else (mkdir (P.dir d); F.mkDir d)

    (* generalized F.rename that works across different file systems *)
    fun rename { old, new } =
	let fun copy () =
		let val ins = BinIO.openIn old
		    val outs = BinIO.openOut new
		    fun loop () =
			let val v = BinIO.input ins
			in if Word8Vector.length v = 0 then
			       (BinIO.closeIn ins; BinIO.closeOut outs)
			   else (BinIO.output (outs, v); loop ())
			end
		in loop ()
		end
	in F.rename { old = old, new = new }
	   handle _ => (* probably on different filesys *)
		  (copy (); rmfile old)
	end

    fun add_anchor (f, a) =
	let val s = TextIO.openAppend f
	in TextIO.output (s, concat [a, " ", a, "\n"]);
	   TextIO.closeOut s
	end

    (* src is still Unix-style, tgt is native: *)
    fun install (usrc, libdir, rtgt) =
	(if CM.stabilize false usrc then
	     case #arcs (P.fromString rtgt) of
		 anchor :: _ =>
		   let val pathconfig =
			   case OS.Process.getEnv "CM_PATHCONFIG" of
			       SOME pc => pc
			     | NONE => P.concat (libdir, "pathconfig")
		       val src = P.fromUnixPath usrc
		       val srcdir = P.dir src
		       val srcfile = P.file src
		       val s_src = pconc [srcdir, CM.cm_dir_arc,
					  arch_oskind, srcfile]
		       val tgt = P.concat (libdir, rtgt)
		       val { dir = tgtdir, file = tgtfile } = P.splitDirFile tgt
		       val s_tgtdir = pconc [tgtdir, CM.cm_dir_arc, arch_oskind]
		       val s_tgt = P.concat (s_tgtdir, tgtfile)
		   in mkdir s_tgtdir;
		      rename { old = s_src, new = s_tgt };
		      add_anchor (pathconfig, anchor);
		      (* TODO: uniqconfig *)
		      OS.Process.success
		   end
	       | [] => (usage (); OS.Process.failure)
	 else OS.Process.failure)
	handle exn => fail ["uncaught exception: ",
			    General.exnMessage exn, "\n"]

    fun doit [src, libdir, tgt] = install (src, libdir, P.fromUnixPath tgt)
      | doit _ = (usage (); OS.Process.failure)

    (* run the installer *)
    val _ = OS.Process.exit (doit (CommandLine.arguments ()))
end
