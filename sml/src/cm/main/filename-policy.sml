(*
 * A type representing different choices for file naming conventions.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature FILENAMEPOLICY = sig

    type policy

    val colocate :
	{ arch: string, os: SMLofNJ.SysInfo.os_kind } -> policy
    val separate :
	{ root: AbsPath.t, parentArc: string, absArc: string } -> policy

    val mkBinPath : policy -> AbsPath.t -> AbsPath.t
    val mkSkelPath : policy -> AbsPath.t -> AbsPath.t
    val mkStablePath : policy -> AbsPath.t -> AbsPath.t
end

structure FilenamePolicy :> FILENAMEPOLICY = struct

    type converter = AbsPath.t -> AbsPath.t

    type policy = { bin: converter, skel: converter, stable: converter }

    fun kind2name SMLofNJ.SysInfo.BEOS = "beos"
      | kind2name SMLofNJ.SysInfo.MACOS = "macos"
      | kind2name SMLofNJ.SysInfo.OS2 = "os2"
      | kind2name SMLofNJ.SysInfo.UNIX = "unix"
      | kind2name SMLofNJ.SysInfo.WIN32 = "win32"

    fun cmpath d s = let
	val { dir = d0, file = f } = AbsPath.splitDirFile s
	val d1 = AbsPath.joinDirFile { dir = d0, file = "CM" }
	val d2 = AbsPath.joinDirFile { dir = d1, file = d }
    in
	AbsPath.joinDirFile { dir = d2, file = f }
    end

    fun colocate { arch, os } = let
	val archos = concat [arch, "-", kind2name os]
    in
	{ skel = cmpath "SKEL", bin = cmpath archos, stable = cmpath archos }
    end

    fun separate { root, parentArc, absArc } = let
	val root = AbsPath.context root
	fun sep p = let
	    val s = AbsPath.name p
	    fun cvt arc = if arc = OS.Path.parentArc then parentArc else arc
	in
	    case OS.Path.fromString s of
		{ isAbs = false, vol = "", arcs } =>
		    AbsPath.native { context = root,
				     spec = OS.Path.toString
				         { isAbs = false, vol = "",
					   arcs = map cvt arcs } }
	      | _ => AbsPath.native
		    { context = root,
		      spec = OS.Path.joinDirFile { dir = absArc,
						   file = AbsPath.file p } }
	end
    in
	{ skel = cmpath "SKEL", bin = sep, stable = sep }
    end

    fun mkBinPath (p: policy) s = #bin p s
    fun mkSkelPath (p: policy) s = #skel p s
    fun mkStablePath (p: policy) s = #stable p s
end
