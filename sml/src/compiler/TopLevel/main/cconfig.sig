(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* cconfig.sml *)

signature CCONFIG = sig
  
  type pickle
  type hash

  val pickUnpick :
      { context: StaticEnv.staticEnv, env: StaticEnv.staticEnv } ->
      { hash: hash,
	pickle: pickle,
	exportLvars: Access.lvar list,
	exportPid: PersStamps.persstamp option,
	newenv: StaticEnv.staticEnv }

  val mkMkStamp : unit -> Stamps.generator
end (* signature CCONFIG *)
