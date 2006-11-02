(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* cconfig.sml *)

signature CCONFIG = sig
  
  type pickle
  type hash
  type pid = PersStamps.persstamp
  type guid

  val pickUnpick :
      { context: StaticEnv.staticEnv,
	env: StaticEnv.staticEnv,
	guid: guid } ->
      { pid: hash,
	pickle: pickle,
	exportLvars: Access.lvar list,
	exportPid: pid option,
	newenv: StaticEnv.staticEnv }

  val mkMkStamp : unit -> Stamps.generator
end (* signature CCONFIG *)
