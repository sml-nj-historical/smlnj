(* COPYRIGHT (c) 1996 Bell Laboratories. *)
(* interact.sml *)

functor Interact(EvalLoop : EVALLOOP) : INTERACT =
struct
  exception Interrupt = EvalLoop.Interrupt

 (* This is where CM can install itelf into.  Added for the purpose of
  * autoloading. (blume)
  *)
  type envref = EnvRef.envref

  fun installCompManager m = (#compManagerHook EvalLoop.stdParams) := m

  fun interact() = (
	EvalLoop.interact EvalLoop.stdParams;
	OS.Process.exit OS.Process.success)

  fun useFile (fname: string) =
      (app Control.Print.say ["[opening ",fname,"]\n"];
       EvalLoop.evalStream EvalLoop.stdParams
		  (fname,(TextIO.openIn fname
			  handle e as IO.Io _ =>
			      (app Control.Print.say["[use failed: ",
						     General.exnMessage e,
						     "]\n"];
			       raise ErrorMsg.Error))))

  fun useStream (stream: TextIO.instream) =
    EvalLoop.evalStream EvalLoop.stdParams ("<instream>", stream)

  fun initialize () = (* read $HOME/.smlnjrc, if it exists *)
      let val home = valOf (OS.Process.getEnv "HOME")
          val rcfile = ".smlnjrc"
          val rcpath = OS.Path.joinDirFile {dir=home, file=rcfile}
          val rcin = TextIO.openIn rcpath
          (* when reading the rc file, we use the usual evalloop 
           * params, but we substitute a pretty-printer that 
           * does nothing, so that we get rid of gross 
           * `val it = () : unit' everywhere.
           *)
          val {compManagerHook,
               baseEnvRef,
               localEnvRef,
               transform,
               instrument,
               perform,
               isolate,...} = EvalLoop.stdParams

          val params : EvalLoop.interactParams = 
              {compManagerHook = compManagerHook,
               baseEnvRef = baseEnvRef,
               localEnvRef = localEnvRef,
               transform = transform,
               instrument = instrument,
               perform = perform,
               isolate = isolate,
               printer = fn _ => fn _ => fn _ => () (* no-op printer *)
               }
      in
          EvalLoop.evalStream params (rcpath, rcin)
      end
          handle Option => ()
               | IO.Io _ => ()

  fun evalStream (stream: TextIO.instream, baseEnv: SCEnv.Env.environment) : 
      SCEnv.Env.environment =
      let val r = ref Environment.emptyEnv
	  val localEnvRef = {get=(fn()=> !r),set=(fn x=>r:=x)}
	  val b = ref baseEnv
	  val baseEnvRef = 
            {get=(fn()=> !b),set=(fn _ => raise Fail "evalStream")}
       in EvalLoop.evalStream
	    ({compManagerHook = ref NONE,
	      (* ????  should CM get its hands into that? *)
	      baseEnvRef = baseEnvRef,
	      localEnvRef=localEnvRef,
	      transform=(fn x => x), 
              instrument=(fn _ => fn x => x),
	      perform=(fn x => x),
	      isolate= #isolate EvalLoop.stdParams,
	      printer= #printer EvalLoop.stdParams})
	    ("<instream>", stream);
	  SCEnv.SC (#get localEnvRef ())
      end

  (* These mUse functions should really be part of the Open Compiler *)
  val mUseFile_hiddenList =
        ref [ [((fn () => (print "--mUseFile not reset!")),"Error!")] ];

  fun mUseFile_reset () = (mUseFile_hiddenList := [])
  fun mUseFile_add   f  = (mUseFile_hiddenList := (f::(!mUseFile_hiddenList)))
  fun mUseFile_list  () = (List.rev(!mUseFile_hiddenList))

  fun mUseFile (test) (fname: string) =
      let fun repeat test n = 
               if (test n) then (useFile fname; repeat test (n+1)) else ()
      in repeat test 0 end

end (* functor Interact *)
