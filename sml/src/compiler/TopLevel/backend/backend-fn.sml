(* backend-fn.sml
 * 
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
functor BackendFn (M : CODEGENERATOR) : BACKEND = struct
    structure Interact =
    Interact
	(EvalLoopF
	     (CompileF
		  (structure M = M
		   structure CC : CCONFIG = struct
		       (* configuration for interactive toplevel:
			* no real pickling/unpickling, pids are
			* assigned randomly *)
		       type pickle = unit
		       type hash = unit
		       local
			   val topCount = ref 0
		       in
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
		       end

		       local
			   val stampGen = Stamps.newGenerator ()
		       in
		           fun mkMkStamp () = stampGen (* always the same *)
		       end
		   end)))

    structure Compile =
    CompileF (structure M = M
	      structure CC : CCONFIG = struct
	          (* compiler configuration for batch compilation
		   * (under control of CM); real pickling, unpickling, and
		   * pid-generation *)
	          type pickle = Word8Vector.vector
		  type hash = PersStamps.persstamp

		  fun pickUnpick { context, env = newenv } = let
		      val m = GenModIdMap.mkMap context
		      fun up_context _ = m
		      val { hash, pickle, exportLvars, exportPid } = 
			  PickMod.pickleEnv (PickMod.INITIAL m) newenv
		      val newenv' =
			  UnpickMod.unpickleEnv up_context (hash, pickle)
		  in
		      { hash = hash,
			pickle = pickle,
			exportLvars = exportLvars,
			exportPid = exportPid,
			newenv = newenv' }
		  end

		  val mkMkStamp = Stamps.newGenerator
	      end)

    structure Profile = ProfileFn (ProfEnv (Interact))
    structure Machine = M.Machine
    val architecture = M.architecture
end
