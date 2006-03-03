(* os-process.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * The generic process control interface.
 *)

structure OS_Process : OS_PROCESS =
  struct

    structure S = Scheduler
    structure PM = ProcManager
    structure CC = SMLofNJ.Cont

    structure P = OS.Process
    structure PP = Posix.Process

    type status = P.status

    val success = P.success
    val failure = P.failure

(** NOTE: we probably need to disable timer signals here **)
    fun system' cmd = (
	  S.stopTimer();
	  case PP.fork()
	   of NONE => (
		PP.exec ("/bin/sh", ["sh", "-c", cmd])
		PP.exit 0w127)
	    | (SOME pid) => (S.restartTimer(); pid)
	  (* end case *))

    fun systemEvt cmd = let
	  val pid = system' cmd
	  val evt = (S.atomicBegin(); PM.addPid pid before S.atomicEnd())
	  in
	    Event.wrap (evt,
	      fn PP.W_EXITED => P.success
	       | _ => P.failure)
	  end

    val system = Event.sync o systemEvt

    fun atExit _ = raise Fail "OS.Process.atExit unimplemented"
    fun exit sts = (S.atomicBegin(); CC.throw (!S.shutdownHook) (true, sts))
    fun terminate sts = (S.atomicBegin(); CC.throw (!S.shutdownHook) (false, sts))

    val getEnv = P.getEnv

  end
