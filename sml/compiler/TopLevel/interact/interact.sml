(* COPYRIGHT (c) 1996 Bell Laboratories. *)
(* interact.sml *)

functor Interact(EvalLoop : EVALLOOP) : INTERACT =
struct
  exception Interrupt = EvalLoop.Interrupt

  type envref = EnvRef.envref

  val installCompManagers = EvalLoop.installCompManagers

  fun interact() = (EvalLoop.interact (); OS.Process.exit OS.Process.success)

  val withErrorHandling = EvalLoop.withErrorHandling

  fun useFile fname =
      (app Control.Print.say ["[opening ",fname,"]\n"];
       EvalLoop.evalStream
	   (fname, (TextIO.openIn fname
		    handle e as IO.Io _ =>
			   (app Control.Print.say["[use failed: ",
						  General.exnMessage e,
						  "]\n"];
			    raise ErrorMsg.Error))))

  fun useStream stream = EvalLoop.evalStream ("<instream>", stream)

  fun evalStream (stream, baseEnv) = let
      val r = ref Environment.emptyEnv
      val base = { set = fn _ => raise Fail "evalStream: #set base",
		   get = fn () => baseEnv }
      val loc = { set = fn e => r := e,
		  get = fn () => !r }
      val props = PropList.newHolder ()
      val state = { loc = loc, base = base, props = props }
  in
      EnvRef.locally (state,
		      fn () => (EvalLoop.evalStream ("<instream>", stream);
				!r))
  end

  local open SMLofNJ.Cont
  in val redump_heap_cont : string cont ref =
	 ref (callcc (fn ret => (callcc (fn k => throw ret k);
				 raise Fail "redump_heap_cont init")))
  end

end (* functor Interact *)
