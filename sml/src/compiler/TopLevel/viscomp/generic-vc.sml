(*
 * Copyright 1998 by Bell Laboratories
 *  generic-vc.sml -- machine independent part of viscomp
 *
 * by Matthias Blume (10/1998)
 *)

structure GenericVC : GENERIC_VC = struct
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
    structure PPTable =	struct
	val install_pp 
            : string list -> (PrettyPrint.ppstream -> 'a -> unit) -> unit
	    = Unsafe.cast PPTable.install_pp
    end (* PPTable *)
    structure MakePid = struct
	fun makePid (context, se) =
	    #hash (PickMod.pickleEnv (context, CMStaticEnv.unCM se))
    end
    structure Ast = Ast

    structure PrintHooks : PRINTHOOKS = struct
	fun prAbsyn env d  = 
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
    val version = Version.version
    val banner = Version.banner
end

(*
 * $Log: generic-vc.sml,v $
 * Revision 1.1  1998/10/16 14:04:03  george
 *   Implemented a hierachical bin directory structure and
 *   broke up the Compiler structure into a machine dependent
 *   and independent parts. [blume]
 *
 *)
