(*
 * A type representing different choices for file naming conventions.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature FILENAMEPOLICY = sig

    type policy
    type policyMaker = { arch: string, os: SMLofNJ.SysInfo.os_kind } -> policy

    val colocate : policyMaker
    val separate : { bindir: string, bootdir: string } -> policyMaker

    val colocate_generic : { arch: string, os: string } -> policy
    val separate_generic : { bindir: string, bootdir: string } ->
			   { arch: string, os: string } -> policy

    val mkBinName : policy -> SrcPath.t -> string
    val mkSkelName : policy -> SrcPath.t -> string
    val mkStableName : policy -> SrcPath.t * Version.t option -> string

    val kind2name : SMLofNJ.SysInfo.os_kind -> string
end

functor FilenamePolicyFn (val cmdir : string
			  val versiondir: Version.t -> string
			  val skeldir : string) :> FILENAMEPOLICY = struct

    type policy = { bin: SrcPath.t -> string,
		    skel: SrcPath.t -> string,
		    stable: SrcPath.t * Version.t option -> string }

    type policyMaker = { arch: string, os: SMLofNJ.SysInfo.os_kind } -> policy

    fun kind2name SMLofNJ.SysInfo.BEOS = "beos"
      | kind2name SMLofNJ.SysInfo.MACOS = "macos"
      | kind2name SMLofNJ.SysInfo.OS2 = "os2"
      | kind2name SMLofNJ.SysInfo.UNIX = "unix"
      | kind2name SMLofNJ.SysInfo.WIN32 = "win32"

    fun mkPolicy (shiftbin, shiftstable, ignoreversion) { arch, os } = let
	fun cmname dl s = let
	    val { dir = d0, file = f } = OS.Path.splitDirFile s
	    val d1 = OS.Path.joinDirFile { dir = d0, file = cmdir }
	    fun subDir (sd, d) = OS.Path.joinDirFile { dir = d, file = sd }
	    val d2 = foldl subDir d1 dl
	in
	    OS.Path.joinDirFile { dir = d2, file = f }
	end
	val archos = concat [arch, "-", os]
	val stable0 = cmname [archos] o shiftstable
	val stable =
	    if ignoreversion then stable0 o #1
	    else (fn (s, NONE) => stable0 s
		   | (s, SOME v) => let
			 val try =
			     cmname [versiondir v, archos] (shiftstable s)
			 val exists =
			     OS.FileSys.access (try, []) handle _ => false
		     in
			 if exists then try else stable0 s
		     end)
    in
	{ skel = cmname [skeldir] o SrcPath.osstring,
	  bin = cmname [archos] o shiftbin,
	  stable = stable }
    end

    fun ungeneric g { arch, os } = g { arch = arch, os = kind2name os }

    val colocate_generic = mkPolicy (SrcPath.osstring, SrcPath.osstring, false)

    fun separate_generic { bindir, bootdir } = let
	fun shiftname root p =
	    case SrcPath.reAnchoredName (p, root) of
		SOME s => s
	      | NONE => (Say.say ["Failure: ", SrcPath.descr p,
				  " is not an anchored path!\n"];
			 raise Fail "bad path")
    in
	mkPolicy (shiftname bindir, shiftname bootdir, true)
    end

    val colocate = ungeneric colocate_generic
    val separate = ungeneric o separate_generic

    fun mkBinName (p: policy) s = #bin p s
    fun mkSkelName (p: policy) s = #skel p s
    fun mkStableName (p: policy) (s, v) = #stable p (s, v)
end

structure FilenamePolicy =
    FilenamePolicyFn (val cmdir = "CM" val skeldir = "SKEL"
		      val versiondir = Version.toString)
