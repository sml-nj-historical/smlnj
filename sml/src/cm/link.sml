(* test implementation of functor LinkCM *)

functor LinkCM (structure HostMachDepVC : MACHDEP_VC) = struct

  local
      structure AutoLoad = AutoLoad
      structure YaccTool = YaccTool
      structure LexTool = LexTool
      structure BurgTool = BurgTool
      val _ = EnvConfig.init ()

      structure E = GenericVC.Environment
      structure SE = GenericVC.StaticEnv
      structure ER = GenericVC.EnvRef
      structure BE = GenericVC.BareEnvironment
      structure CMSE = GenericVC.CMStaticEnv
      structure S = GenericVC.Symbol
      structure CoerceEnv = GenericVC.CoerceEnv

      val os = SMLofNJ.SysInfo.getOSKind ()

      structure SSV = SpecificSymValFn (structure MachDepVC = HostMachDepVC
					val os = os)

      (* For testing purposes, I need to have access to the old basis
       * library.  This is done via the "primitives" mechanism.  Eventually,
       * the basis will be accessed as a genuine library. The "primitives"
       * mechanism is really meant to serve a different purpose.
       *
       * We split the existing pervasive environment into two parts:
       *  1. All ML module definitions -- this is the part that will
       *     become available via a primitive called "basis".
       *  2. The remaining (non-modular) bindings.  Those will be
       *     used as our "pervasive" environment.
       *
       * I didn't bother to split dynamic or symbolic environments.
       * To function properly this is not necessary, and the whole thing
       * will soon go away anyhow.
       *)
      fun split e = let
	  val sym = E.symbolicPart e
	  val dyn = E.dynamicPart e
	  val stat = E.staticPart e
	  val bstat = CoerceEnv.es2bs stat
	  fun f ((s, b), (mods, nomods)) =
	      case S.nameSpace s of
		  (S.STRspace | S.SIGspace | S.FCTspace | S.FSIGspace) =>
		      (SE.bind (s, b, mods), nomods)
		| _ => (mods, SE.bind (s, b, nomods))
	  val (bmods, bnomods) = SE.fold f (SE.empty, SE.empty) bstat
	  val mods = CMSE.CM bmods
	  val nomods = CMSE.CM bnomods
	  fun mk s = E.mkenv { static = s, dynamic = dyn, symbolic = sym }
      in
	  { mod = mk mods, nomod = mk nomods }
      end

      val system_values = ref (E.dynamicPart E.emptyEnv)

      (* Instantiate the persistent state functor; this includes
       * the binfile cache and the dynamic value cache *)
      structure FullPersstate =
	  FullPersstateFn (structure MachDepVC = HostMachDepVC
			   val system_values = system_values)

      (* Create two arguments appropriate for being passed to
       * CompileGenericFn. One instantiation of that functor
       * is responsible for "recompile" traversals, the other one
       * does "link" traversals. Notice how the two share the same
       * underlying state. *)
      structure Recomp = RecompFn (structure PS = FullPersstate)
      structure Exec = ExecFn (structure PS = FullPersstate)

      (* make the two traversals *)
      structure RecompTraversal = CompileGenericFn (structure CT = Recomp)
      structure ExecTraversal = CompileGenericFn (structure CT = Exec)

      (* The StabilizeFn functor needs a way of converting bnodes to
       * dependency-analysis environments.  This can be achieved quite
       * conveniently by a "recompile" traversal for bnodes. *)
      fun bn2statenv gp i = #1 (#stat (valOf (RecompTraversal.bnode gp i)))
	  handle Option => raise Fail "bn2statenv"

      (* exec_group is basically the same as ExecTraversal.group with
       * two additional actions to be taken:
       *   1. Before executing the code, we announce the priviliges
       *      that are being invoked.  (For the time being, we assume
       *      that everybody has every conceivable privilege, but at the
       *      very least we announce which ones are being made use of.)
       *   2. After we are done we must make the values of "shared"
       *      compilation units permanent. *)
      fun exec_group gp (g as GroupGraph.GROUP { required = rq, ... }) =
	  (if StringSet.isEmpty rq then ()
	   else Say.say ("$Execute: required privileges are:\n" ::
		     map (fn s => ("  " ^ s ^ "\n")) (StringSet.listItems rq));
	   ExecTraversal.group gp g
	   before FullPersstate.rememberShared ())

      fun recomp_runner gp g = isSome (RecompTraversal.group gp g)

      (* This function combines the actions of "recompile" and "exec".
       * When successful, it combines the results (thus forming a full
       * environment) and adds it to the toplevel environment. *)
      fun make_runner gp g =
	  case RecompTraversal.group gp g of
	      NONE => false
	    | SOME { stat, sym} =>
		  (case exec_group gp g of
		       NONE => false
		     | SOME dyn => let
			   val delta = E.mkenv { static = stat, symbolic = sym,
						 dynamic = dyn }
			   val base = #get ER.topLevel ()
			   val new = BE.concatEnv (CoerceEnv.e2b delta, base)
		       in
			   #set ER.topLevel new;
			   Say.vsay ["[New bindings added.]\n"];
			   true
		       end)

      fun loadit gp m =
	  case RecompTraversal.impexpmap gp m of
	      NONE => NONE
	    | SOME { stat, sym } => let
		  fun exec () =
		      ExecTraversal.impexpmap gp m
		      before FullPersstate.rememberShared ()
	      in
		  case exec () of
		      NONE => NONE
		    | SOME dyn => let
			  val e = E.mkenv { static = stat, symbolic = sym,
					    dynamic =dyn }
			  val be = GenericVC.CoerceEnv.e2b e
		      in
			  SOME be
		      end
	      end

      val theParam = ref (NONE: GeneralParams.param option)
      fun param () =
	  case !theParam of
	      SOME p => p
	    | NONE => let
		  val { mod = basis, nomod = perv } =
		      split (#get ER.pervasive ())
		  val corenv = #get ER.core ()
		  val bpspec = let
		      val bogus = GenericVC.PersStamps.fromBytes
			  (Byte.stringToBytes "0123456789abcdef")
		  in
		      { name = "basis",
		        env = basis,
			pidInfo = { statpid = bogus, sympid = bogus,
				    ctxt = GenericVC.CMStaticEnv.empty } }
		  end
		  val primconf = Primitive.configuration [bpspec]
		  val pcmode = PathConfig.new ()
		  val _ = PathConfig.set (pcmode,
					  "smlnj-lib.cm",
					  "/home/blume/ML/current/lib")
		  val fnpolicy =
		      FilenamePolicy.colocate
		          { os = os, arch = HostMachDepVC.architecture }
		  val keep_going = EnvConfig.getSet StdConfig.keep_going NONE
		  val p = { primconf = primconf,
			    fnpolicy = fnpolicy,
			    pcmode = pcmode,
			    symenv = SSV.env,
			    keep_going = keep_going,
			    pervasive = perv,
			    corenv = corenv,
			    pervcorepids = PidSet.empty }
	      in
		  theParam := SOME p;
		  p
	      end

      val al_greg = GroupReg.new ()
      fun al_ginfo () = { param = param (),
			  groupreg = al_greg,
			  errcons = GenericVC.ErrorMsg.defaultConsumer () }

      val al_manager = AutoLoad.mkManager (fn m => loadit (al_ginfo ()) m)

      fun manager (ast, _, ter) = al_manager (ast, ter)

      val _ = HostMachDepVC.Interact.installCompManager (SOME manager)

      (* Instantiate the stabilization mechanism. *)
      structure Stabilize =
	  StabilizeFn (val bn2statenv = bn2statenv
		       val getPid = FullPersstate.pid_fetch_sml
		       val recomp = recomp_runner
		       val transfer_state = FullPersstate.transfer_state)

      (* Access to the stabilization mechanism is integrated into the
       * parser. I'm not sure if this is the cleanest way, but it works
       * well enough. *)
      structure Parse = ParseFn (structure Stabilize = Stabilize
				 val pending = AutoLoad.getPending)

      (* this is just a dummy argument to "run" (see below). *)
      fun stabilize_runner gp g = true
  in
    structure CM = struct

	fun run sflag f s = let
	    val c = SrcPath.cwdContext ()
	    val p = SrcPath.native { context = c, spec = s }
	in
	    case Parse.parse NONE (param ()) sflag p of
		NONE => false
	      | SOME (g, gp) => f gp g
	end

	fun stabilize recursively = run (SOME recursively) stabilize_runner
	val recomp = run NONE recomp_runner
	val make = run NONE make_runner

	fun autoload s = let
	    val c = SrcPath.cwdContext ()
	    val p = SrcPath.native { context = c, spec = s }
	in
	    case Parse.parse (SOME al_greg) (param ()) NONE p of
		NONE => false
	      | SOME (g, _) =>
		    (AutoLoad.register (GenericVC.EnvRef.topLevel, g);
		     true)
	end
    end

    structure CMB = struct
	local
	    structure BC =
		BootstrapCompileFn (structure MachDepVC = HostMachDepVC
				    val os = os)
	in
	    open BC
	    fun setRetargetPervStatEnv x = ()
	    val make' = deliver'
	    val wipeOut = reset
	end
    end
  end
end

signature CMTOOLS = sig end
signature COMPILATION_MANAGER = sig end
