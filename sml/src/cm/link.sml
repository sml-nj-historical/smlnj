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

      fun doall farsbnode (GroupGraph.GROUP { exports, ... }, gp) = let
	  fun one ((fsbn, _), false) = false
	    | one ((fsbn, _), true) =
	      isSome (farsbnode gp fsbn)
      in
	  SymbolMap.foldl one true exports
      end

      val recomp_group = doall RecompTraversal.farsbnode
      fun exec_group arg =
	  (DynTStamp.new ();
	   doall ExecTraversal.farsbnode arg)
      fun make_group arg =
	  (if recomp_group arg then exec_group arg else false)
  in
    structure CM = struct

	fun run f s = let
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
	in
	    Say.vsay "[starting]\n";
	    Option.map f (CMParse.parse param p)
	end

	val parse = run #1
	val recomp = run recomp_group
	val make = run make_group
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
