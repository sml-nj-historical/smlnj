(* machine-gen.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generation of machine code from a list of CPS functions
 *)

signature MACHINE_GEN =
  sig

    include MACHINE

    structure MLTreeComp : MLTREECOMP
		  where CFG = CFG
		    and I = CFG.I
    structure InvokeGC   : INVOKE_GC
		  where CFG=MLTreeComp.CFG
		    and TS = MLTreeComp.TS
    structure Shuffle    : SHUFFLE
		  where I = MLTreeComp.I
    structure MachSpec   : MACH_SPEC

    val abi_variant : string option (* to distinguish between different ABIs
				     * for same CPU/OSKind combination;
				     * prime example: intel-based macs which
				     * are x86/unix vs. intel-based linux
				     * boxen. *)

    val codegen : {
	    funcs: CPS.function list,
	    limits: CPS.lvar -> int * int,
	    source: string
	  } -> (unit -> int)

  end (* MACHINE_GEN *)
