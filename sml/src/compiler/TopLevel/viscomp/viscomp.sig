(* COPYRIGHT (c) 1996 Bell Laboratories. *)
(* viscomp.sig *)

signature PRINTHOOKS =
sig
  (* all output goes to Control.Print.out *)
  val prAbsyn : StaticEnv.staticEnv -> Absyn.dec -> unit
end

signature VISCOMP = 
sig 
  structure Stats : STATS
  structure Control : CONTROL
  structure Source : SOURCE
  structure SourceMap : SOURCE_MAP
  structure ErrorMsg : ERRORMSG
  structure Symbol : SYMBOL
  structure StaticEnv : STATICENV
  structure DynamicEnv : DYNENV
  structure BareEnvironment : ENVIRONMENT
  structure Environment : ENVIRONMENT = CMEnv.Env
  structure CoerceEnv : COERCE_ENV
  structure EnvRef : ENVREF
  structure ModuleId : MODULE_ID
  structure CMStaticEnv : CMSTATICENV
  structure Profile : PROFILE
  structure Binfile: BINFILE
  structure CMSA: CMSA
  structure PersStamps : PERSSTAMPS
  structure PrettyPrint : PRETTYPRINT
  structure PPTable : 
   sig
    val install_pp : string list -> 
                      (PrettyPrint.ppstream -> 'a -> unit) -> unit
   end
  structure Ast : AST
  structure Compile : COMPILE
  structure Interact : INTERACT
(*
  structure AllocProf : sig val reset : unit -> unit
			    val print : outstream -> unit
			end
*)
  structure PrintHooks : PRINTHOOKS
  structure Boot : sig val coreEnvRef : CMEnv.Env.environment ref end
  val version : {
          system : string,      	(* the system title *)
	  version_id : int list,	(* the version number *)
          date : string         	(* date of creation *)
	}
  val banner : string
  val architecture: string
end  


(*
 * $Log: viscomp.sig,v $
 * Revision 1.3  1998/05/21 17:54:49  jhr
 *   Merging in Matthias's changes.
 *
 * Revision 1.2  1998/05/15 03:54:08  dbm
 *   Eliminate FixityParse and LazyComp.
 *
 * Revision 1.1.1.1  1998/04/08 18:39:14  george
 * Version 110.5
 *
 *)
