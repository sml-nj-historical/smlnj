(*
 * Copyright 1996 by Bell Laboratories
 *  boot.sml -- bootstrap environments
 *
 *   completely redone by M.Blume (5/1998)
 *)
signature BOOTENV = sig
    val bootEnv: unit -> Environment.environment
end

functor BootEnvF (structure BF: BINFILE
		  val architecture: string
		  val setRetargetPervStatEnv: CMStaticEnv.staticEnv -> unit
		  val cmbmake: string -> unit) :> BOOTENV = struct

    exception BootFailure

    structure Env = Environment
    structure CMS = CMStaticEnv
    structure SE = StaticEnv
    structure DynE = DynamicEnv
    structure PS = PersStamps

    datatype envrequest = BARE | NORMAL | FULL

    fun say s = (Control.Print.say s; Control.Print.flush ())
    fun die s = (say s; raise BootFailure)

    (* just run CMB.make to make a new set of binfiles... *)
    fun recompile bindir =
	(say (concat ["***** BUILDING NEW BINFILES IN ", bindir, " *****\n"]);
	 cmbmake bindir;
	 say "***** NEW BINFILES ARE READY *****\n")

    (* get the dynamic environment from the runtime system *)
    local
	structure U = Unsafe

	(* Here we collect all export pids as they come from the
	 * environments.  They may differ from those in the dynamic
	 * environment because of "rebuild". *)
	val stpids = ref ([]: PS.persstamp list)
    in
	fun regStPid (SOME p) = stpids := p :: !stpids
	  | regStPid NONE = ()

	fun getdyn rebuilt = let
	    (* ignore the very last pid -- it's the runtime system! *)
	    fun convert (U.CONSrde (_, _, U.NILrde), [_]) = DynE.empty
	      | convert (U.CONSrde (rawdynpid, obj, rest), stpid :: pids) = let
		    val dynpid = PS.fromBytes rawdynpid
		    val _ =
			if rebuilt orelse stpid = dynpid then ()
			else die (concat
			    ["Pid mismatch: static = ", PS.toHex stpid,
			     ", dynamic = ", PS.toHex dynpid, "\n"])
		in
		    DynE.bind (stpid, obj, convert (rest, pids))
		end
	      | convert _ = die "Pid list mismatch\n"
	    val rde = !U.pStruct before U.pStruct := U.NILrde
	    val stpids = !stpids before stpids := []
	in
	    convert (rde, stpids)
	end
    end

    (* get the boot environments from the binfiles *)
    fun fetchBootEnv (bindir, ereq, rebuilt) = let

	fun b name = OS.Path.joinDirFile { dir = bindir, file = name }

	(* parse all listfiles *)
	local
	    fun readListFile listname = let
		val name = b listname
		val f = TextIO.openIn name
		val whole = TextIO.inputAll f before TextIO.closeIn f
	    in
		String.tokens (fn c => c = #"\n") whole
	    end
	in
	    (* strip "core.sml.bin" *)
	    val bootlist = List.tl (readListFile "BOOTLIST")
	    val pervlist = readListFile "PERVLIST"
	    (* strip theGlue.sml.bin -- still executing *)
	    val complist = rev (List.tl (rev (readListFile "COMPLIST")))
	end

	(* get pid, statenv, and symenv from one binfile *)
	fun getbin (context, binname) = let
	    val name = b binname
	    val _ = say (concat ["Loading from ", name, "..."])
	    val f = BinIO.openIn name
	    val bfc = BF.read { name = name, stream = f,
			        senv = context, keep_code = false }
	    val senv = BF.senvOf bfc
	    val symenv = BF.symenvOf bfc
	in
	    BinIO.closeIn f;
	    regStPid (BF.exportPidOf bfc);
	    say "done.\n";
	    (senv, symenv)
	end

	infix //
	fun (st1, sy1) // (st2, sy2) =
	    (CMS.atop (st1, st2), Env.layerSymbolic (sy1, sy2))

	local
	    fun pb x =
		OS.Path.toString { isAbs = false, vol = "",
				   arcs = ["PervEnv", "Boot", x] }
	in
	    (* magic file names -- live in PervEnv/Boot *)
	    val assembly_sig = pb "assembly.sig.bin"
	    val dummy_sml = pb "dummy.sml.bin"
	    val core_sml = pb "core.sml.bin"
	end

	val emptysym = Env.symbolicPart Env.emptyEnv

	(* build the core environment *)
	val _ = say "----- CORE ENVIRONMENT -----\n"
	val prim = (CMS.CM PrimEnv.primEnv, emptysym)
	val sig_prim = getbin (#1 prim, assembly_sig) // prim
	val dummy_env = getbin (#1 sig_prim, dummy_sml) // sig_prim
	val core_env = getbin (#1 dummy_env, core_sml)
	val _ = #set EnvRef.core (CMS.unCM (#1 core_env))

	(* loading many sources successively, layering envs as one goes *)
	fun many (files, scontext) = let
	    fun loop ([], env) = env
	      | loop (f1 :: files, env) =
		loop (files, getbin (CMS.atop (#1 env, scontext), f1) // env)
	in
	    loop (files, (CMS.empty, emptysym))
	end

	(* load all the files from BOOTLIST *)
	val _ = say "----- BOOT -----\n"
	val bootenv = many (bootlist, #1 (core_env // sig_prim))
	(* load all the files from PERVLIST -- this gives the pervasive env *)
	val _ = say "----- PERVASIVE ENVIRONMENT -----\n"
	val (pervstatenv, pervsymenv) = many (pervlist, #1 bootenv)

	(* register this environment as the base environment for "retarget" *)
	val _ = setRetargetPervStatEnv pervstatenv

	(* now get the environment for the compiler *)
	val _ = say "----- COMPILER MODULES -----\n"
	val (compstatenv, compsymenv) = many (complist, pervstatenv)

	(* strip modmaps from static envs *)
	val pervstatenv = CMS.unCM pervstatenv
	val compstatenv = CMS.unCM compstatenv

	(* add stuff to the pervasive env (depends on "ereq" flag) *)
	val _ = say "----- COMPILER BINDINGS -----\n"
	val pervstatenv =
	    let
		(* looking for symbols in compiler's static env *)
		fun complook sym =
		    SE.look (compstatenv, sym)
		    handle e =>
			(say (concat ["Unbound: ",
				      Symbol.symbolToString sym, "\n"]);
			 raise e)

		(* rebinding a symbol from the compiler's static env *)
		fun comprebind (sym, e) =
		    (say (concat ["Rebind: ",
				  Symbol.symbolToString sym, "\n"]);
		     SE.bind (sym, complook sym, e))

		(* rebind a structure with all its signatures *)
		fun comprebind_strsym (sym, e) = let
		    val b = complook sym
		    val str = case b of
			Bindings.STRbind str => str
		      | _ => die "boot.sml: bad structure binding!\n"
		    val sigsyms = ModuleUtil.getSignatureNames str
		in
		    foldl comprebind (SE.bind (sym, b, e)) sigsyms
		end

		(* function for just rebinding the visible compiler *)
		fun rebind_viscomp () =
		    comprebind_strsym (Symbol.strSymbol "Compiler",
				       pervstatenv)
	    in
		case ereq of
		    BARE => rebind_viscomp () (* no CM, CMB, ... *)
		  | FULL =>
			(* "full" compiler ->
			 *         put everything and the kitchen sink into
			 *         the pervasive environment *)
			SE.atop (compstatenv, pervstatenv)
		  | NORMAL => let
		    (* normal compile ->
		     *             put only a few bindings into pervenv *)
		    in
			foldl comprebind (rebind_viscomp ())
			      [Symbol.strSymbol "CM",
			       Symbol.strSymbol "CMB",
			       Symbol.sigSymbol "CMTOOLS",
			       Symbol.sigSymbol "COMPILATION_MANAGER"]
		    end
	    end

	(* consolidating static part *)
	val pervstatenv = SE.consolidate pervstatenv

	(* dynamic and symbolic parts *)
	(* hack: dynamic and symbolic parts of the pervasive env contain
	 *       bindings responsible for stuff in core env! *)
	val (dynamic, symbolic) = let
	    val fulldynenv = getdyn rebuilt
	    val fullsymenv =
		Env.layerSymbolic (compsymenv,
				   Env.layerSymbolic (pervsymenv, #2 core_env))
	    val trimstatic = SE.atop (pervstatenv, CMS.unCM (#1 core_env))
	    val tobetrimmed = { static = trimstatic,
			        dynamic = fulldynenv, symbolic = fullsymenv }
	    val { dynamic, symbolic, ... } = Env.trimEnv tobetrimmed
	in
	    (dynamic, symbolic)
	end
    in
	(* putting things together *)
	say "+++++ ENVIRONMENTS BUILT +++++\n";
	{ static = pervstatenv, dynamic = dynamic, symbolic = symbolic }
    end

    fun bootEnv () = let
	(* grab relevant command line arguments... *)
	fun vArg (prefix, arg) =
	    if String.isPrefix prefix arg then
		SOME (String.extract (arg, size prefix, NONE))
	    else NONE
	fun bootArgs ([], bootdir, newbindir, ereq) =
	    (bootdir, newbindir, ereq)
	  | bootArgs ("@SMLfull" :: rest, bootdir, newbindir, _) =
	    bootArgs (rest, bootdir, newbindir, FULL)
	  | bootArgs ("@SMLbare" :: rest, bootdir, newbindir, _) =
	    bootArgs (rest, bootdir, newbindir, BARE)
	  | bootArgs (head :: rest, bootdir, newbindir, ereq) =
	    (case vArg ("@SMLboot=", head) of
		 SOME bootdir =>
		     bootArgs (rest, bootdir, newbindir, ereq)
	       | NONE => (case vArg ("@SMLrebuild=", head) of
			      newbindir as SOME _ =>
				  bootArgs (rest, bootdir, newbindir, ereq)
			    | NONE =>
				  bootArgs (rest, bootdir, newbindir, ereq)))

	val (bootdir, newbindir, ereq) =
	    bootArgs (SMLofNJ.getAllArgs (),
		      "bin." ^ architecture, NONE, NORMAL)
	val bootdir = OS.Path.mkCanonical bootdir
	val newbindir = Option.map OS.Path.mkCanonical newbindir

	(* determine where the good binfiles are *)
	val (goodbindir, rebuilt) =
	    case newbindir of
		NONE => (bootdir, false)
	      | SOME nbd => if nbd = bootdir then
		    die "@SMLboot= and @SMLrebuild= name the same directory\n"
		  else (recompile nbd; (nbd, true))
    in
	fetchBootEnv (goodbindir, ereq, rebuilt)
    end
end
