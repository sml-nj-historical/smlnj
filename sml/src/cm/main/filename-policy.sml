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
    val separate : string -> policyMaker

    val mkBinPath : policy -> AbsPath.t -> AbsPath.t
    val mkSkelPath : policy -> AbsPath.t -> AbsPath.t
    val mkStablePath : policy -> AbsPath.t -> AbsPath.t
end

functor FilenamePolicyFn (val cmdir : string
			  val skeldir : string) :> FILENAMEPOLICY = struct

    type converter = AbsPath.t -> AbsPath.t

    type policy = { bin: converter, skel: converter, stable: converter }
    type policyMaker = { arch: string, os: SMLofNJ.SysInfo.os_kind } -> policy

    fun kind2name SMLofNJ.SysInfo.BEOS = "beos"
      | kind2name SMLofNJ.SysInfo.MACOS = "macos"
      | kind2name SMLofNJ.SysInfo.OS2 = "os2"
      | kind2name SMLofNJ.SysInfo.UNIX = "unix"
      | kind2name SMLofNJ.SysInfo.WIN32 = "win32"

    fun mkPolicy shift { arch, os } = let
	fun cmpath d s = let
	    val { dir = d0, file = f } = AbsPath.splitDirFile s
	    val d1 = AbsPath.joinDirFile { dir = d0, file = cmdir }
	    val d2 = AbsPath.joinDirFile { dir = d1, file = d }
	in
	    AbsPath.joinDirFile { dir = d2, file = f }
	end
	val archos = concat [arch, "-", kind2name os]
	val archosdep = cmpath archos o shift
    in
	{ skel = cmpath skeldir, bin = archosdep, stable = archosdep }
    end

    val colocate = mkPolicy (fn p => p)

    fun separate root = let
	fun shift p =
	    case AbsPath.reAnchor (p, root) of
		SOME p' => p'
	      | NONE => (Say.say ["Failure: ", AbsPath.name p,
				  " is not an anchored path!\n"];
			 raise Fail "bad path")
    in
	mkPolicy shift
    end

    fun mkBinPath (p: policy) s = #bin p s
    fun mkSkelPath (p: policy) s = #skel p s
    fun mkStablePath (p: policy) s = #stable p s
end

structure FilenamePolicy =
    FilenamePolicyFn (val cmdir = "NEWCM" val skeldir = "SKEL")
