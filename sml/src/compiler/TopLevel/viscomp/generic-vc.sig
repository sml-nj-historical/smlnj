(*
 * Copyright 1998 by Bell Laboratories
 *  generic-vc.sig -- machine independent part of viscomp
 *
 * by Matthias Blume (10/1998)
 *)

signature PRINTHOOKS = sig
    (* all output goes to Control.Print.out *)
    val prAbsyn : StaticEnv.staticEnv -> Absyn.dec -> unit
end

signature GENERIC_VC = sig
    structure Stats : STATS
    structure Control : CONTROL
    structure Source : SOURCE
    structure SourceMap : SOURCE_MAP
    structure ErrorMsg : ERRORMSG
    structure Symbol : SYMBOL
    structure SymPath : SYMPATH
    structure StaticEnv : STATICENV
    structure DynamicEnv : DYNENV
    structure BareEnvironment : ENVIRONMENT
    structure Environment : ENVIRONMENT = CMEnv.Env
    structure CoerceEnv : COERCE_ENV
    structure EnvRef : ENVREF
    structure ModuleId : MODULE_ID
    structure CMStaticEnv : CMSTATICENV
    structure PersStamps : PERSSTAMPS
    structure PrettyPrint : PRETTYPRINT
    structure PPTable : sig
	val install_pp : string list -> 
	    (PrettyPrint.ppstream -> 'a -> unit) -> unit
    end
    structure MakePid : sig
	val makePid : CMStaticEnv.staticEnv * CMStaticEnv.staticEnv
	    -> PersStamps.persstamp
    end
    structure Ast : AST
    structure SmlFile : SMLFILE

    structure PrintHooks : PRINTHOOKS

    val version : {
		   system : string,      	(* the system title *)
		   version_id : int list,	(* the version number *)
		   date : string         	(* date of creation *)
		   }
    val banner : string
end


(*
 * $Log: generic-vc.sig,v $
 * Revision 1.1  1998/10/16 14:04:03  george
 *   Implemented a hierachical bin directory structure and
 *   broke up the Compiler structure into a machine dependent
 *   and independent parts. [blume]
 *
 *)
