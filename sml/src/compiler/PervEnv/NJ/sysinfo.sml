(* sysinfo.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Get information about the underlying hardware and OS.
 *
 *)

structure SysInfo : SYS_INFO =
  struct

    exception UNKNOWN

    fun getInfoStr NONE = raise UNKNOWN
      | getInfoStr (SOME s) = s

    datatype os_kind
      = UNIX	(* one of the many flavours of UNIX (incl Mach and NeXTStep) *)
      | WIN32	(* Wind32 API (incl. Windows95 and WindowsNT) *)
      | MACOS	(* Macintosh OS *)
      | OS2	(* IBM's OS/2 *)
      | BEOS	(* BeOS from Be *)

    fun sysInfo (s: string): string option =
	  CInterface.c_function "SMLNJ-RunT" "sysInfo" s
    fun getFlag flag = (case (getInfoStr(sysInfo flag))
	   of "NO" => false
	    | _ => true
	  (* end case *))

    fun getOSName () = getInfoStr(sysInfo "OS_NAME")
    fun getOSKind () = (case (getOSName())
	   of ("SunOS"|"Solaris"|"Irix"|"OSF/1"|"AIX"|"SVR4"|"NeXTStep"
	      |"Ultrix"|"HPUX"|"Linux"|"BSD"|"PLAN9"|"MACH"
	      ) => UNIX
	    | "OS/2" => OS2
	    | "Win32" => WIN32
	    | _ => raise Fail "unknown OS"
	  (* end case *))
    fun getOSVersion () = getInfoStr(sysInfo "OS_VERSION")

    fun getHostArch () = getInfoStr(sysInfo "HOST_ARCH")
    fun getTargetArch () = getInfoStr(sysInfo "TARGET_ARCH")

    fun hasSoftwarePolling () = getFlag "HAS_SOFT_POLL"
    fun hasMultiprocessing () = getFlag "HAS_MP"

  end


(*
 * $Log: sysinfo.sml,v $
 * Revision 1.2  1997/01/26 19:05:35  lorenz
 * add win32 mapping
 *
 * Revision 1.1.1.1  1997/01/14  01:38:20  george
 *   Version 109.24
 *
 *)
