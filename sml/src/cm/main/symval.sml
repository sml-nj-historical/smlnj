(*
 * Implementation of environments for CM "preprocessor" variables.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature SYMVAL = sig

    type env

    val look : env -> string -> int option
    val empty : env

    val default : { arch: string,
		    big: bool,
		    size: int,
		    os: SMLofNJ.SysInfo.os_kind,
		    version: int list }
	-> env
end

structure SymVal :> SYMVAL = struct

    type env = int StringMap.map

    fun look e s = StringMap.find (e, s)

    val empty = StringMap.empty

    fun default { arch, big, size, os, version } = let
	val arch_sym = "ARCH_" ^ arch
	val endian_sym = if big then "BIG_ENDIAN" else "LITTLE_ENDIAN"
	val size_sym = "SIZE_" ^ Int.toString size
	val os_sym = case os of
	    SMLofNJ.SysInfo.UNIX => "OPSYS_UNIX"
	  | SMLofNJ.SysInfo.WIN32 => "OPSYS_WIN32"
	  | SMLofNJ.SysInfo.MACOS => "OPSYS_MACOS"
	  | SMLofNJ.SysInfo.OS2 => "OPSYS_OS2"
	  | SMLofNJ.SysInfo.BEOS => "OPSYS_BEOS"
	val (major, minor) =
	    case version of
		[] => (0, 0)
	      | [major] => (major, 0)
	      | major :: minor :: _ => (major, minor)
	val major_sym = "SMLNJ_VERSION"
	val minor_sym = "SMLNJ_MINOR_VERSION"

	val alldefs = [(arch_sym, 1),
		       (endian_sym, 1),
		       (size_sym, 1),
		       (os_sym, 1),
		       (major_sym, major),
		       (minor_sym, minor),
		       ("NEW_CM", 1)]
    in
	foldl StringMap.insert' empty alldefs
    end
end
