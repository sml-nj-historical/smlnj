(* win32-pre-sock.sml
 *
 * COPYRIGHT (c) 1999 Bell Labs, Lucent Technologies.
 *
 * Provide some utility operations for CML sockets.
 *)

structure PreSock : sig

    datatype socket_state
      = Unconnected	(* initial state *)
      | Connecting	(* when waiting for a connect to complete *)
      | Connected	(* when connected *)
      | Accepting	(* when waiting for an accept to complete *)
      | WaitingOnIO	(* when waiting on an input and/or output operation *)
      | Closed

    datatype ('a, 'b) sock = CMLSock of {
	state : socket_state SyncVar.mvar,
	sock : ('a, 'b) Socket.sock
      }

    val mkSock : ('a, 'b) Socket.sock -> ('a, 'b) sock

    val wouldBlock : ('a -> 'b) -> 'a -> 'b option
	(* attempt the system call; return SOME x, if it succeeds with x, return
	 * NONE, if it fails because it would have blocked.
	 *)

    val inEvt : ('a, 'b) sock -> unit CML.event
    val outEvt : ('a, 'b) sock -> unit CML.event

  end = struct

    datatype socket_state
      = Unconnected	(* initial state *)
      | Connecting	(* when waiting for a connect to complete *)
      | Connected	(* when connected *)
      | Accepting	(* when waiting for an accept to complete *)
      | WaitingOnIO	(* when waiting on an input and/or output operation *)
      | Closed

    datatype ('a, 'b) sock = CMLSock of {
	state : socket_state SyncVar.mvar,
	sock : ('a, 'b) Socket.sock
      }

  (* given an SML socket, return a CML socket (which is non-blocking) *)
    fun mkSock s = (
	  Socket.Ctl.setNBIO(s, true);
	  CMLSock{
	      state = SyncVar.mVarInit Unconnected,
	      sock = s
	    })



(* For now, just return true -- figure out how to differentiate error
 * messages though
 *)
    fun blockErr (OS.SysErr _) = true

(*
    val blockErrors = (case Posix.Error.syserror "wouldblock"
	   of NONE => [Posix.Error.again, Posix.Error.inprogress]
	    | (SOME e) => [e, Posix.Error.again, Posix.Error.inprogress]
	  (* end case *))

    fun blockErr (OS.SysErr(_, SOME err)) = let
	  fun isErr [] = false
	    | isErr (e::r) = (e = err) orelse isErr r
	  in
	    isErr blockErrors
	  end
*)


    fun wouldBlock f x = SOME(f x)
	  handle ex => if (blockErr ex) then NONE else raise ex

    fun inEvt (CMLSock{sock, ...}) =
	  CML.wrap(IOManager.ioEvt(OS.IO.pollIn(Socket.pollDesc sock)), ignore)
    fun outEvt (CMLSock{sock, ...}) =
	  CML.wrap(IOManager.ioEvt(OS.IO.pollOut(Socket.pollDesc sock)), ignore)

  end;

