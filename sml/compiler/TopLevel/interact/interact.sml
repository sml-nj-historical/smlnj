(* interact.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor Interact(EvalLoop : EVALLOOP) : INTERACT =
  struct
    exception Interrupt = EvalLoop.Interrupt

    type envref = EnvRef.envref

    val installCompManagers = EvalLoop.installCompManagers

    fun interact() = (EvalLoop.interact (); OS.Process.exit OS.Process.success)

    val withErrorHandling = EvalLoop.withErrorHandling

  (* compile a file; returns true if okay and false on error *)
    fun useFile fname = let
	  val _ = app Control.Print.say ["[opening ", fname, "]\n"]
	  val strm = TextIO.openIn fname
		handle e as IO.Io _ => (
		  app Control.Print.say["[use failed: ", General.exnMessage e, "]\n"];
		  raise ErrorMsg.Error)
	  in
	    (EvalLoop.evalStream (fname, strm); true) handle _ => false
	  end

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
	    EnvRef.locally (state, fn () => (EvalLoop.evalStream ("<instream>", stream); !r))
	  end

    local
      open SMLofNJ.Cont
    in
      val redump_heap_cont : string cont ref =
	    ref (callcc (fn ret => (callcc (fn k => throw ret k);
				    raise Fail "redump_heap_cont init")))
    end

  end (* functor Interact *)
