(* COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies *)
(* binfile.sig *)

signature BINFILE = sig

    exception FormatError
    exception NoCodeBug
    exception NoPickleBug

    exception Compile of string
    exception TopLevelException of exn
    exception SilentException

    type bfContent

    type pid = PersStamps.persstamp
    type senv = CMEnv.Env.staticEnv
    type symenv = CMEnv.Env.symenv
    type denv = CMEnv.Env.dynenv
    type env = CMEnv.Env.environment

    val staticPidOf: bfContent -> pid
    val exportPidOf: bfContent -> pid option
    val lambdaPidOf: bfContent -> pid
    val cmDataOf: bfContent -> pid list
    val senvOf: bfContent -> senv
    val symenvOf: bfContent -> symenv

    val discardCode: bfContent -> unit
    val noCode: bfContent -> bool

    val create: { runtimePid: pid option,
		  splitting: bool,
		  cmData: pid list,
		  ast: Ast.dec,
		  source: Source.inputSource,
		  senv:  senv,
		  symenv: symenv,
		  corenv: EnvRef.staticEnv }
	-> bfContent

    val read: { name: string,
	        stream: BinIO.instream,
		senv: senv,
		keep_code: bool }
	-> bfContent

    val write: { stream: BinIO.outstream,
		 content: bfContent,
		 keep_code: bool }
	-> unit

    val exec: bfContent * denv -> env
end (* signature BINFILE *)

(*
 * $Log: binfile.sig,v $
 * Revision 1.3  1998/10/16 14:03:43  george
 *   Implemented a hierachical bin directory structure and
 *   broke up the Compiler structure into a machine dependent
 *   and independent parts. [blume]
 *
 * Revision 1.2  1998/05/23 14:10:20  george
 *   Fixed RCS keyword syntax
 *
 *)
