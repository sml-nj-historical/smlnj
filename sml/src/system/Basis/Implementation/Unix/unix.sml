(* unix.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Unix : UNIX =
  struct

    structure P = Posix.Process
    structure PS = POSIX_Signal
    structure PE = Posix.ProcEnv
    structure PF = Posix.FileSys
    structure PIO = Posix.IO
    structure SS = Substring

    type signal = PS.signal
    datatype exit_status = datatype P.exit_status

    datatype ('a, 'b) proc =
	     PROC of { base: string,
		       pid  : P.pid,
		       infd : PIO.file_desc,
		       outfd: PIO.file_desc,
		       (* The following two elements are a temporary
			* hack until the general interface is sorted out.
			* The idea is to have "reap" close the most recently
			* created stream.  If no stream has been created,
			* then the file descriptors get closed directly. *)
		       closein: (unit -> unit) ref,
		       closeout: (unit -> unit) ref,
		       exit_status: OS.Process.status option ref }

    val fromStatus = P.fromStatus

    fun protect f x = let
          val _ = Signals.maskSignals Signals.MASKALL
          val y = (f x) handle ex => 
                    (Signals.unmaskSignals Signals.MASKALL; raise ex)
          in
            Signals.unmaskSignals Signals.MASKALL; y
          end

    fun fdTextReader (name : string, fd : PIO.file_desc) =
	  PosixTextPrimIO.mkReader {
              initBlkMode = true,
              name = name,
              fd = fd
            }

    fun fdBinReader (name : string, fd : PIO.file_desc) =
	  PosixBinPrimIO.mkReader {
              initBlkMode = true,
              name = name,
              fd = fd
            }

    fun fdTextWriter (name, fd) =
          PosixTextPrimIO.mkWriter {
	      appendMode = false,
              initBlkMode = true,
              name = name,
              chunkSize=4096,
              fd = fd
            }

    fun fdBinWriter (name, fd) =
          PosixBinPrimIO.mkWriter {
	      appendMode = false,
              initBlkMode = true,
              name = name,
              chunkSize=4096,
              fd = fd
            }

    fun openTextOutFD (name, fd) =
	  TextIO.mkOutstream (
	    TextIO.StreamIO.mkOutstream (
	      fdTextWriter (name, fd), IO.BLOCK_BUF))

    fun openBinOutFD (name, fd) =
	  BinIO.mkOutstream (
	    BinIO.StreamIO.mkOutstream (
	      fdBinWriter (name, fd), IO.BLOCK_BUF))

    fun openTextInFD (name, fd) =
	  TextIO.mkInstream (
	    TextIO.StreamIO.mkInstream (
	      fdTextReader (name, fd), ""))

    fun openBinInFD (name, fd) =
	  BinIO.mkInstream (
	    BinIO.StreamIO.mkInstream (
	      fdBinReader (name, fd), Byte.stringToBytes ""))

    fun setcloser (r, f, s) = (r := (fn () => f s); s)

    fun textInstreamOf (PROC { base, infd, closein, ... }) =
	setcloser (closein, TextIO.closeIn,
		   openTextInFD (base ^ "_exec_txt_in", infd))
    fun binInstreamOf (PROC { base, infd, closein, ... }) =
	setcloser (closein, BinIO.closeIn,
		   openBinInFD (base ^ "_exec_bin_in", infd))
    fun textOutstreamOf (PROC { base, outfd, closeout, ... }) =
	setcloser (closeout, TextIO.closeOut,
		   openTextOutFD (base ^ "_exec_txt_out", outfd))
    fun binOutstreamOf (PROC { base, outfd, closeout, ... }) =
	setcloser (closeout, BinIO.closeOut,
		   openBinOutFD (base ^ "_exec_bin_out", outfd))

    fun streamsOf p = (textInstreamOf p, textOutstreamOf p)

    fun executeInEnv (cmd, argv, env) =
	let val p1 = PIO.pipe ()
            val p2 = PIO.pipe ()
            fun closep () =
		(PIO.close (#outfd p1); 
                 PIO.close (#infd p1);
                 PIO.close (#outfd p2); 
                 PIO.close (#infd p2))
            val base = SS.string(SS.taker (fn c => c <> #"/") (SS.all cmd))
            fun startChild () =
		case protect P.fork () of
		    SOME pid =>  pid           (* parent *)
		  | NONE =>
		    let val oldin = #infd p1
			val newin = Posix.FileSys.wordToFD 0w0
			val oldout = #outfd p2
			val newout = Posix.FileSys.wordToFD 0w1
                    in
			PIO.close (#outfd p1);
			PIO.close (#infd p2);
			if (oldin = newin) then ()
			else (PIO.dup2{old = oldin, new = newin};
                              PIO.close oldin);
			if (oldout = newout) then ()
			else (PIO.dup2{old = oldout, new = newout};
                              PIO.close oldout);
			P.exece (cmd, base::argv, env)
		    end
            val _ = TextIO.flushOut TextIO.stdOut
            val pid = (startChild ()) handle ex => (closep(); raise ex)
	    val infd = #infd p2
	    val outfd = #outfd p1
	in
            (* close the child-side fds *)
            PIO.close (#outfd p2);
            PIO.close (#infd p1);
            (* set the fds close on exec *)
            PIO.setfd (#infd p2, PIO.FD.flags [PIO.FD.cloexec]);
            PIO.setfd (#outfd p1, PIO.FD.flags [PIO.FD.cloexec]);
            PROC { base = base, pid = pid, infd = infd, outfd = outfd,
		   closein = ref (fn () => PIO.close infd),
		   closeout = ref (fn () => PIO.close outfd),
		   exit_status = ref NONE }
	end

    fun execute (cmd, argv) = executeInEnv (cmd, argv, PE.environ())

    fun kill (PROC{pid,...},signal) = P.kill (P.K_PROC pid, signal)

    fun reap (PROC { exit_status = ref (SOME s), ... }) = s
      | reap (PROC { exit_status, pid, closein, closeout, ... }) =
	let
            (* protect is probably too much; typically, one
             * would only mask SIGINT, SIGQUIT and SIGHUP
             *)
            fun waitProc () =
		case #2(protect P.waitpid (P.W_CHILD pid,[])) of
		    W_EXITED => 0
		  | W_EXITSTATUS s => Word8Imp.toInt s
		  | W_SIGNALED (PS.SIG s) => 256 + s
		  | W_STOPPED (PS.SIG s) => (* should not happen! *) 512 + s
	    val _ = !closein ()
	    val _ = !closeout () handle _ => ()
	    val s = waitProc ()
        in
	    exit_status := SOME s;
	    s
        end

    val exit = P.exit

  end (* structure Unix *)

