(* io-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * extracted from io.mldoc (v. 1.4; 1997-01-21)
 *)

signature IO =
  sig
    exception Io of {name : string, function : string, cause : exn}
    exception BlockingNotSupported
    exception NonblockingNotSupported
    exception RandomAccessNotSupported
    exception TerminatedStream
    exception ClosedStream
    datatype buffer_mode = NO_BUF | LINE_BUF | BLOCK_BUF
    
  end
