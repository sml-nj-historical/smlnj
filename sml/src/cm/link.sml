(* test implementation of functor LinkCM *)

functor LinkCM (structure HostMachDepVC : MACHDEP_VC) = struct

  local
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

      fun split e = let
	  val sym = E.symbolicPart e
	  val dyn = E.dynamicPart e
	  val stat = E.staticPart e
	  val bstat = CMSE.unCM stat
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

      structure FullPersstate =
	  FullPersstateFn (structure MachDepVC = HostMachDepVC)

      structure Recomp = RecompFn (structure PS = FullPersstate)
      structure Exec = ExecFn (structure PS = FullPersstate)

      structure RecompTraversal = CompileGenericFn (structure CT = Recomp)
      structure ExecTraversal = CompileGenericFn (structure CT = Exec)

      fun bn2statenv gp i = #1 (#stat (valOf (RecompTraversal.bnode gp i)))

      val recomp_group = RecompTraversal.group

      fun exec_group gp (g as GroupGraph.GROUP { required = rq, ... }) =
	  (if StringSet.isEmpty rq then ()
	   else Say.say ("$Execute: required privileges are:\n" ::
		     map (fn s => ("  " ^ s ^ "\n")) (StringSet.listItems rq));
	   ExecTraversal.group gp g
	   before FullPersstate.rememberShared ())

      fun recomp_runner gp g = isSome (recomp_group gp g)

      fun make_runner gp g =
	  case recomp_group gp g of
	      NONE => false
	    | SOME { stat, sym} =>
		  (case exec_group gp g of
		       NONE => false
		     | SOME dyn => let
			   val delta = E.mkenv { static = stat, symbolic = sym,
						 dynamic = dyn }
			   val base = #get ER.topLevel ()
			   val new = BE.concatEnv (ER.unCMenv delta, base)
		       in
			   #set ER.topLevel new;
			   Say.vsay ["[New bindings added.]\n"];
			   true
		       end)

      structure Stabilize =  StabilizeFn (val bn2statenv = bn2statenv
					  val recomp = recomp_runner)

      structure Parse = ParseFn (structure Stabilize = Stabilize)

      fun stabilize_runner gp g = true
  in
    structure CM = struct

	fun run sflag f s = let
	    val c = AbsPath.cwdContext ()
	    val p = AbsPath.native { context = AbsPath.cwdContext (),
				     spec = s }
	    val { mod = basis, nomod = perv } =
		split (#get ER.pervasive ())
	    val corenv = #get ER.core ()
	    val primconf = Primitive.configuration { basis = basis }
	    val param = { primconf = primconf,
			  fnpolicy = FilenamePolicy.default,
			  keep_going = true,
			  pervasive = perv,
			  corenv = corenv }
	in
	    case Parse.parse param sflag p of
		NONE => false
	      | SOME (g, gp) => f gp g
	end

	fun stabilize recursively = run (SOME recursively) stabilize_runner
	val recomp = run NONE recomp_runner
	val make = run NONE make_runner
    end

    structure CMB = struct
	fun setRetargetPervStatEnv x = ()
	fun wipeOut () = ()
	fun make' _ = ()
    end
  end
end

signature CMTOOLS = sig end
signature COMPILATION_MANAGER = sig end
