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

    val parse: Source.inputSource -> Ast.dec
    val makePid: senv * senv -> pid
end (* signature BINFILE *)

(*
 * $Log$
 *)
