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
		       val stampGen = Stamps.newGenerator ()
		       val stampConv = Stamps.newConverter ()
		       fun mkMkStamp () = stampGen (* always the same *)
		       type guid = unit
		       local
			   val topCount = ref 0
		       in
		           fun pickUnpick { context, env = newenv, guid } = let
			       val _ = topCount := !topCount + 1
			       val { newenv = newenv', hash,
				     exportLvars, hasExports } =
				   PickMod.dontPickle { env = newenv,
							count = !topCount }
			       fun stamp2string s =
				   Stamps.Case stampConv s
				    { fresh = fn i =>
						 "toplevel::" ^ Int.toString i,
				      global = fn { pid, cnt } =>
						  concat [PersStamps.toHex pid,
							  ":",
							  Int.toString cnt],
				      special = fn s => s }
			   in
			       { pid = (),
				 pickle = (),
				 exportLvars = exportLvars,
				 exportPid = if hasExports then SOME hash
					     else NONE,
				 newenv = newenv',
				 stamp2string = stamp2string }
			   end
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
		  type guid = string

		  fun pickUnpick { context, env = newenv, guid } = let
		      val m = GenModIdMap.mkMap context
		      fun up_context _ = m
		      val { hash, pickle, exportLvars, hasExports,
			    stampConverter } = 
			  PickMod.pickleEnv (PickMod.INITIAL m) newenv
		      val pid = Rehash.addGUID { hash = hash, guid = guid }
		      val newenv' =
			  UnpickMod.unpickleEnv up_context (pid, pickle)
		      fun stamp2string s =
			  let fun pair (pid, i) =
				  concat [PersStamps.toHex pid, ":",
					  Int.toString i]
			  in
			      Stamps.Case stampConverter s
					  { fresh = fn i => pair (pid, i),
					    global = fn { pid, cnt } =>
							pair (pid, cnt),
					    special = fn s => s }
			  end
		  in
		      { pid = pid,
			pickle = pickle,
			exportLvars = exportLvars,
			exportPid = if hasExports then SOME pid else NONE,
			newenv = newenv',
			stamp2string = stamp2string }
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
