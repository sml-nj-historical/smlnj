signature SYMVAL = sig

    type env

    val look : env -> string -> int option
    val empty : env

    val default : { arch: string,
		    extra_arch: string option,
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

    fun default { arch, extra_arch, big, size, os, version } = let
	fun mk_arch_sym a = "ARCH_" ^ a
	val arch_sym = mk_arch_sym arch
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

	val almost_alldefs = [(arch_sym, 1),
			      (endian_sym, 1),
			      (size_sym, 1),
			      (os_sym, 1),
			      (major_sym, major),
			      (minor_sym, minor)]

	val alldefs =
	    case extra_arch of
		NONE => almost_alldefs
	      | SOME a => (mk_arch_sym a, 1) :: almost_alldefs
    in
	foldl StringMap.insert' empty alldefs
    end
end
