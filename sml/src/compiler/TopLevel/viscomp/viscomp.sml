(* COPYRIGHT (c) 1996 Bell Laboratories. *)
(* viscomp.sml *)

functor VisComp(Machm : CODEGENERATOR) : VISCOMP =
struct
  structure Stats = Stats
  structure Control = Control
  structure Source = Source
  structure SourceMap = SourceMap
  structure ErrorMsg = ErrorMsg
  structure Symbol = Symbol
  structure StaticEnv = StaticEnv
  structure DynamicEnv = DynamicEnv
  structure BareEnvironment = Environment
  structure Environment = SCEnv.Env
  structure CoerceEnv = CoerceEnv
  structure EnvRef = EnvRef
  structure ModuleId = ModuleId
  structure SCStaticEnv = SCStaticEnv
  structure PersStamps = PersStamps
  structure PrettyPrint = PrettyPrint
  structure PPTable =
    struct
      val install_pp : string list -> (PrettyPrint.ppstream -> 'a -> unit) -> unit
	    = Unsafe.cast PPTable.install_pp
    end (* PPTable *)
  structure Ast = Ast
  structure LazyComp = LazyComp 
  structure FixityParse = FixityParse
  structure Lambda = Lambda
  structure CompileArg = struct
      structure Machm = Machm
      exception SilentException	  (* shared between Compile and TopCompile *)
      structure SCS = SCStaticEnv
      structure Pickles = 
          struct type pickle = Word8Vector.vector
	         type hash = PersStamps.persstamp
                 fun pickUnpick(compenv,newenv) =
                      let val {hash,pickle,exportLvars,exportPid} = 
                              PickMod.pickleEnv(compenv,newenv)
                          val newenv' = UnpickMod.unpickleEnv(compenv, 
						    {hash=hash,pickle=pickle})
                       in {hash=hash,pickle=pickle,exportLvars=exportLvars,
			   exportPid=exportPid,newenv=newenv'}
                      end
         end
      val mkMkStamp = Stamps.new
  end
  structure Compile = CompileF (CompileArg)
  structure TopCompileArg = struct
      open CompileArg
      structure SCS = struct type staticEnv=StaticEnv.staticEnv
                             fun unSC x = x  fun SC x = x
                      end
      structure Pickles = struct
            type pickle = unit
            type hash = unit
            val topCount = ref 0
            fun pickUnpick(compenv,newenv) =
               let val _ = topCount := !topCount + 1
		   val (newenv',hash,exportLvars,exportPid) = PickMod.dontPickle(newenv,!topCount)
                in {hash=(),pickle=(),exportLvars=exportLvars,
		    exportPid=exportPid,newenv=newenv'}
               end
      end
      local val mkStamp = Stamps.new()
       in fun mkMkStamp() = mkStamp 
      end
  end
  structure Interact = Interact(EvalLoopF(CompileF(TopCompileArg)))
  structure Machm = Machm
  structure Profile = ProfileFn(ProfEnv(Interact))
  structure CUnitUtil = CUnitUtilFun(structure Compile=Compile
                                     structure Machm=Machm)
  val architecture = Machm.architecture
  structure CMSA = CMSAFun (structure CUnitUtil = CUnitUtil
			    structure Compile = Compile
			    val arch = architecture)
(*
  structure AllocProf =
    struct
      val reset = AllocProf.reset
      val print = AllocProf.print_profile_info
    end
*)
  structure PrintHooks : PRINTHOOKS =
    struct fun prAbsyn env d  = 
	       PrettyPrint.with_pp (ErrorMsg.defaultConsumer())
	                 (fn ppstrm => PPAbsyn.ppDec(env,NONE) ppstrm (d,200))
	   fun prLambda lexp = (MCprint.printLexp lexp; print "\n")
	   fun prLamFun lexp v = (MCprint.printFun lexp v; print "\n")
    end

(*  functor Debugger = RealDebugger *)
  structure Boot = struct val coreEnvRef = ref(SCEnv.Env.emptyEnv) end
  val version = Version.version
  val banner = Version.banner

end (* functor VisComp *)

