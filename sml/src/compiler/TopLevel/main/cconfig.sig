(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* cconfig.sml *)

signature CCONFIG = 
sig
  exception SilentException
  
  type statenv = StaticEnv.staticEnv
   
  type pickle
  type hash
  type lvar = Access.lvar
  type pid = PersStamps.persstamp

  val pickUnpick : { context: statenv, env: statenv } ->
		   { hash: hash,
		     pickle: pickle,
		     exportLvars: lvar list,
		     exportPid: pid option,
		     newenv: statenv }

  val mkMkStamp : unit -> Stamps.generator
end (* signature CCONFIG *)
