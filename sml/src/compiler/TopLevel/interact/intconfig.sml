(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* intconfig.sml *)

structure IntConfig : CCONFIG = 
struct
  fun bug s = ErrorMsg.impossible ("IntConfig:" ^ s)
  exception SilentException = BatchConfig.SilentException
  
  type statenv = StaticEnv.staticEnv
   
  type pickle = unit
  type hash = unit
  type lvar = Access.lvar
  type pid = PersStamps.persstamp

  val topCount = ref 0

  fun pickUnpick { context, env = newenv } = let
      val _ = topCount := !topCount + 1
      val (newenv', hash, exportLvars, exportPid) = 
	  PickMod.dontPickle (newenv, !topCount)
  in
      { hash = (),
        pickle = (),
	exportLvars = exportLvars,
	exportPid = exportPid,
	newenv = newenv' }
  end

  val stampGen = Stamps.newGenerator ()
  val mkMkStamp = fn () => stampGen	(* always the same *)
end (* structure IntConfig *)

