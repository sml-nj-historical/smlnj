(* COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies *)
(* batchutil.sig *)

signature BATCHUTIL =
  sig
    exception FormatError
    exception NoCodeBug

    type 'iid cunit
    type pid = PersStamps.persstamp

    type senv = SCEnv.Env.staticEnv
    type symenv = SCEnv.Env.symenv
    type denv = SCEnv.Env.dynenv
    type env = SCEnv.Env.environment

    type csegments = {c0: Word8Vector.vector, 
                      cn: Word8Vector.vector list, 
                      name: string option ref}

    val readUnit: { name: string,
                    stream: BinIO.instream,
		    pids2iid: pid list -> 'iid,
		    senv: senv,
		    keep_code: bool }
	-> 'iid cunit
    val writeUnit: { stream: BinIO.outstream,
		     cunit: 'iid cunit,
		     keep_code: bool,
		     iid2pids: 'iid -> pid list }
	-> unit

    val staticPidCU: 'iid cunit -> pid
    val lambdaPidCU: 'iid cunit -> pid
    val senvCU: 'iid cunit -> senv
    val symenvCU: 'iid cunit -> symenv
    val envCU: 'iid cunit -> env option ref
    val referencesCU: 'iid cunit -> 'iid
    val nocodeCU: 'iid cunit -> bool
    val exportCU: 'iid cunit -> pid option
    val discardCode: 'iid cunit -> unit

    exception Compile of string
    exception TopLevelException of exn
    exception SilentException
    val arch :  string
    val parse: Source.inputSource -> Ast.dec
    val makePid: senv * senv -> pid
    val makeUnit: { runtimePid: pid option,
                    splitting: bool,
                    references: 'iid,
                    ast: Ast.dec,
                    source: Source.inputSource,
                    senv:  senv,
                    symenv: symenv,
                    corenv: EnvRef.staticEnv } -> 'iid cunit
    val execUnit: 'iid cunit * denv -> env

  end (* signature BATCHUTIL *)
