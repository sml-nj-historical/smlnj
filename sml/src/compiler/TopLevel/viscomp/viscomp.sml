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
  structure Environment = CMEnv.Env
  structure CoerceEnv = CoerceEnv
  structure EnvRef = EnvRef
  structure ModuleId = ModuleId
  structure CMStaticEnv = CMStaticEnv
  structure PersStamps = PersStamps
  structure PrettyPrint = PrettyPrint
  structure PPTable =
    struct
      val install_pp 
            : string list -> (PrettyPrint.ppstream -> 'a -> unit) -> unit
	    = Unsafe.cast PPTable.install_pp
    end (* PPTable *)
  structure Ast = Ast

  structure Interact = Interact(EvalLoopF(CompileF(structure M=Machm
                                                   structure CC=IntConfig)))
  structure Compile = CompileF(structure M=Machm
                               structure CC=BatchConfig)
  structure Binfile = BinfileFun (Compile)
  structure CMSA = CMSAFun (structure BF = Binfile
                            structure C = Compile)

  structure Profile = ProfileFn(ProfEnv(Interact))
  structure PrintHooks : PRINTHOOKS =
    struct fun prAbsyn env d  = 
	       PrettyPrint.with_pp (ErrorMsg.defaultConsumer())
	                 (fn ppstrm => PPAbsyn.ppDec(env,NONE) ppstrm (d,200))
    end
(*
  structure AllocProf =
    struct
      val reset = AllocProf.reset
      val print = AllocProf.print_profile_info
    end
*)
  structure Boot = struct val coreEnvRef = ref(CMEnv.Env.emptyEnv) end
  val version = Version.version
  val banner = Version.banner
  val architecture = Machm.architecture
end (* functor VisComp *)


(*
 * $Log: viscomp.sml,v $
 * Revision 1.3  1998/05/21 17:54:51  jhr
 *   Merging in Matthias's changes.
 *
 * Revision 1.2  1998/05/15 03:54:31  dbm
 *   Eliminate FixityParse and LazyComp.
 *
 * Revision 1.1.1.1  1998/04/08 18:39:14  george
 * Version 110.5
 *
 *)
