(* backend-fn.sml
 * 
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
functor BackendFn (structure M : CODEGENERATOR
		   val cproto_conv : string) : BACKEND = struct
    structure Interact =
    Interact
	(EvalLoopF
	     (CompileF
		  (val cproto_conv = cproto_conv
		   structure M = M
		   structure CC : CCONFIG = struct
		       (* configuration for interactive toplevel:
			* no real pickling/unpickling, pids are
			* assigned randomly *)
		       type pickle = unit
		       type hash = unit
		       type pid = PersStamps.persstamp
		       local
			   val topCount = ref 0
		       in
		           fun pickUnpick { context, env = newenv,
					    uniquepid = _ } = let
			       val _ = topCount := !topCount + 1
			       val { newenv = newenv', hash,
				     exportLvars, hasExports } = 
				   PickMod.dontPickle { env = newenv,
							count = !topCount }
			   in
			       { pid = (),
				 fingerprint = (),
				 pepper = "",
				 pickle = (),
				 exportLvars = exportLvars,
				 exportPid = if hasExports then SOME hash
					     else NONE,
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
    CompileF (val cproto_conv = cproto_conv
              structure M = M
	      structure CC : CCONFIG = struct
	          (* compiler configuration for batch compilation
		   * (under control of CM); real pickling, unpickling, and
		   * pid-generation *)
	          type pickle = Word8Vector.vector
		  type hash = PersStamps.persstamp
		  type pid = hash

		  fun pickUnpick { context, env = newenv, uniquepid } = let
		      val m = GenModIdMap.mkMap context
		      fun up_context _ = m
		      val { hash, pickle, exportLvars, hasExports } = 
			  PickMod.pickleEnv (PickMod.INITIAL m) newenv
		      val (pid, pepper) = uniquepid hash
		      val newenv' =
			  UnpickMod.unpickleEnv up_context (pid, pickle)
		  in
		      { pid = pid,
			fingerprint = hash,
			pepper = pepper,
			pickle = pickle,
			exportLvars = exportLvars,
			exportPid = if hasExports then SOME pid else NONE,
			newenv = newenv' }
		  end

		  val mkMkStamp = Stamps.newGenerator
	      end)

    structure Profile =
        ProfileFn (structure ProfEnv =
		   ProfEnvFn (type env = Environment.environment
			      val staticPart = Environment.staticPart
			      val layer = Environment.concatEnv
			      fun eval (s, e) =
				  Interact.evalStream (TextIO.openString s, e))
		   val pervasive = EnvRef.pervasive)
    structure Machine = M.Machine
    val architecture = M.architecture
end
