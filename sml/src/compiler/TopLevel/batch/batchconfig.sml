(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* batchconfig.sml *)

structure BatchConfig: CCONFIG = 
struct
  exception SilentException
  
  type statenv = StaticEnv.staticEnv
   
  type pickle = Word8Vector.vector
  type hash = PersStamps.persstamp
  type lvar = Access.lvar
  type pid = PersStamps.persstamp

  fun pickUnpick { context, env = newenv } = let
      val m = GenModIdMap.mkMap context
      fun up_context _ = m
      val { hash, pickle, exportLvars, exportPid } = 
	  PickMod.pickleEnv (PickMod.INITIAL m) newenv
      val newenv' = UnpickMod.unpickleEnv up_context (hash, pickle)
  in
      { hash = hash,
        pickle = pickle,
	exportLvars = exportLvars,
	exportPid = exportPid,
	newenv = newenv' }
  end

  val mkMkStamp = Stamps.newGenerator
end (* structure BatchConfig *)
