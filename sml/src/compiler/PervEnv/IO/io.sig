(* io-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature IO =
  sig

    exception Io of {
	name : string,
	function : string,
	cause : exn
      }

    exception BlockingNotSupported
    exception NonblockingNotSupported
    exception RandomAccessNotSupported
    exception TerminatedStream
    exception ClosedStream

    datatype buffer_mode = NO_BUF | LINE_BUF | BLOCK_BUF

  end


(*
 * $Log: io-sig.sml,v $
 * Revision 1.1.1.1  1997/01/14 01:38:18  george
 *   Version 109.24
 *
 *)
