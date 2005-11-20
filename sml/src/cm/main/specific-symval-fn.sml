(*
 * Building a host/OS-specific environments for CM "preprocessor" variables.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
functor SpecificSymValFn (val arch: string
			  val os: SMLofNJ.SysInfo.os_kind) =
    struct
	local
	    val (arch, big, size) =
		case arch of
		    "sparc" => ("SPARC", true, 32)
		  | "alpha32" => ("ALPHA", false, 32)
		  | "mipsel" => ("MIPS", false, 32)
		  | "mipseb" => ("MIPS", true, 32)
		  | "x86" => ("X86", false, 32)
		  | "hppa" => ("HPPA", false, 32)
		  | "rs6000" => ("RS6000", false, 32)
		  | "ppc" => ("PPC", true, 32)
		  | arch => ErrorMsg.impossible
				("unknown architecture: " ^ arch)
	    val env0 = SymVal.default
			   { arch = arch, big = big, size = size, os = os,
			     version = #version_id SMLNJVersion.version }
	    val er = ref env0
	in
	    fun symval s = let
		fun get () = SymVal.look (!er) s
		fun set v = er := SymVal.define (!er, s, v)
	    in
		{ get = get, set = set }
	    end
	end
    end
