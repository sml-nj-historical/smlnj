(* COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies *)
(* binfile.sig *)

signature BINFILE = sig

    exception FormatError

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
    val senvOf: bfContent -> { env: senv, ctxt: ModuleId.Set.set }
    val symenvOf: bfContent -> symenv

    val size: { content: bfContent, nopickle: bool } -> int

    val create: { runtimePid: pid option,
		  splitting: bool,
		  cmData: pid list,
		  ast: Ast.dec,
		  source: Source.inputSource,
		  senv:  senv,
		  symenv: symenv,
		  corenv: EnvRef.staticEnv }
	-> bfContent

    val read:
	{ name: string, stream: BinIO.instream, senv: senv } -> bfContent

    val write:
	{ stream: BinIO.outstream, content: bfContent, nopickle: bool } -> unit

    val exec: bfContent * denv -> denv
end (* signature BINFILE *)

