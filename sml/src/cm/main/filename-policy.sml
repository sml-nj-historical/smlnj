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

    val mkBinName : policy -> SrcPath.t -> string
    val mkSkelName : policy -> SrcPath.t -> string
    val mkStableName : policy -> SrcPath.t -> string
end

functor FilenamePolicyFn (val cmdir : string
			  val skeldir : string) :> FILENAMEPOLICY = struct

    type converter = SrcPath.t -> string

    type policy = { bin: converter, skel: converter, stable: converter }
    type policyMaker = { arch: string, os: SMLofNJ.SysInfo.os_kind } -> policy

    fun kind2name SMLofNJ.SysInfo.BEOS = "beos"
      | kind2name SMLofNJ.SysInfo.MACOS = "macos"
      | kind2name SMLofNJ.SysInfo.OS2 = "os2"
      | kind2name SMLofNJ.SysInfo.UNIX = "unix"
      | kind2name SMLofNJ.SysInfo.WIN32 = "win32"

    fun mkPolicy shiftname { arch, os } = let
	fun cmname d s = let
	    val { dir = d0, file = f } = OS.Path.splitDirFile s
	    val d1 = OS.Path.joinDirFile { dir = d0, file = cmdir }
	    val d2 = OS.Path.joinDirFile { dir = d1, file = d }
	in
	    OS.Path.joinDirFile { dir = d2, file = f }
	end
	val archos = concat [arch, "-", kind2name os]
	val skel = cmname skeldir o SrcPath.osstring
	val archosdep = cmname archos o shiftname
    in
	{ skel = skel, bin = archosdep, stable = archosdep }
    end

    val colocate = mkPolicy SrcPath.osstring

    fun separate root = let
	fun shiftname p =
	    case SrcPath.reAnchoredName (p, root) of
		SOME s => s
	      | NONE => (Say.say ["Failure: ", SrcPath.descr p,
				  " is not an anchored path!\n"];
			 raise Fail "bad path")
    in
	mkPolicy shiftname
    end

    fun mkBinName (p: policy) s = #bin p s
    fun mkSkelName (p: policy) s = #skel p s
    fun mkStableName (p: policy) s = #stable p s
end

structure FilenamePolicy =
    FilenamePolicyFn (val cmdir = "NEWCM" val skeldir = "SKEL")
