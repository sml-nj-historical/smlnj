(* dummy implementation of functor LinkCM *)

functor LinkCM (structure HostMachDepVC : MACHDEP_VC) = struct

  local
      structure YaccTool = YaccTool
      structure LexTool = LexTool
      structure BurgTool = BurgTool
      val _ = EnvConfig.init ()

      structure E = GenericVC.Environment
      structure SE = GenericVC.StaticEnv
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

      fun doall farsbnode (GroupGraph.GROUP { exports, ... }, gp) = let
	  fun one ((fsbn, _), false) = false
	    | one ((fsbn, _), true) =
	      isSome (farsbnode gp fsbn)
      in
	  SymbolMap.foldl one true exports
      end

      val recomp_group = doall RecompTraversal.farsbnode
      fun exec_group arg =
	  (doall ExecTraversal.farsbnode arg)
	  before FullPersstate.rememberShared ()
      fun make_group arg =
	  (if recomp_group arg then exec_group arg else false)

      structure Stabilize =  StabilizeFn (val bn2statenv = bn2statenv
					  val recomp = recomp_group)

      structure Parse = ParseFn (structure Stabilize = Stabilize)
  in
    structure CM = struct

	fun run sflag f s = let
	    val c = AbsPath.cwdContext ()
	    val p = AbsPath.native { context = AbsPath.cwdContext (),
				     spec = s }
	    val { mod = basis, nomod = perv } =
		split (#get GenericVC.EnvRef.pervasive ())
	    val corenv = #get GenericVC.EnvRef.core ()
	    val primconf = Primitive.configuration { basis = basis }
	    val param = { primconf = primconf,
			  fnpolicy = FilenamePolicy.default,
			  keep_going = false,
			  pervasive = perv,
			  corenv = corenv }
	    val g = Parse.parse param sflag p
	in
	    Option.map f g
	end

	fun stabilize recursively = run (SOME recursively) #1
	val recomp = run NONE recomp_group
	val make = run NONE make_group
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
