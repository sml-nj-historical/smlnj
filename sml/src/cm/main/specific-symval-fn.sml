(*
 * Building a host/OS-specific environments for CM "preprocessor" variables.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
functor SpecificSymValFn (structure MachDepVC: MACHDEP_VC
			  val os: SMLofNJ.SysInfo.os_kind) =
    struct
	local
	    val (arch, extra_arch, big, size) =
		case MachDepVC.architecture of
		    "sparc" => ("SPARC", NONE, true, 32)
		  | "alpha32" => ("ALPHA", NONE, false, 32)
		  | "alpha32x" => ("ALPHA", SOME "ALHPA_OLD", false, 32)
		  | "mipsel" => ("MIPS", NONE, false, 32)
		  | "mipseb" => ("MIPS", NONE, true, 32)
		  | "x86" => ("X86", NONE, false, 32)
		  | "hppa" => ("HPPA", NONE, false, 32)
		  | "rs6000" => ("RS6000", NONE, false, 32)
		  | "ppc" => ("PPC", NONE, false, 32)
		  | arch => GenericVC.ErrorMsg.impossible
			("unknown architecture: " ^ arch)
	in
	    val env =
		SymVal.default { arch = arch,
				 extra_arch = extra_arch,
				 big = big,
				 size = size,
				 os = os,
				 version = #version_id GenericVC.version }
	end
    end
