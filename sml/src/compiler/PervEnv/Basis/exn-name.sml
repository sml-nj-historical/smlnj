(* exn-name.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Eventually, this should move to PreBasis so that we don't need the PreGeneral
 * structure anymore.
 *
 *)

structure ExnName : sig

    val exnName : exn -> string
    val exnMessage : exn -> string

  end = struct

    val exnName : exn -> string = InlineT.cast(fn (ref s, _,_) => s)

    fun exnMessage (OS.SysErr(s, NONE)) =
	  "SysErr: " ^ s
      | exnMessage (OS.SysErr(s, SOME e)) =
	  concat["SysErr: ", s, " [", OS.errorName e, "]"]
      | exnMessage (IO.Io{cause, function, name}) = let
	  val causeMsg = (case cause
		 of (OS.SysErr(s, _)) => [", ", s]
		  | IO.BlockingNotSupported => [", blocking I/O not supported"]
		  | IO.NonblockingNotSupported =>
		      [", non-blocking I/O not supported"]
		  | IO.RandomAccessNotSupported => [", random access not supported"]
		  | IO.TerminatedStream => [", terminated input stream"]
		  | IO.ClosedStream => [", closed stream"]
		  | _ => [" with exception ", exnMessage cause]
		(* end case *))
	  in
	    concat("Io: " :: function :: " failed on \"" :: name :: "\"" :: causeMsg)
	  end
      | exnMessage (Fail s) = "Fail: " ^ s
(** NOTE: we should probably include line/file info for Match and Bind *)
      | exnMessage Bind = "nonexhaustive binding failure"
      | exnMessage Match = "nonexhaustive match failure"
      | exnMessage Subscript = "subscript out of bounds"
      | exnMessage Size = "size"
      | exnMessage Overflow = "overflow"
      | exnMessage Div = "divide by zero"
      | exnMessage Domain = "domain error"
      | exnMessage e = exnName e

  end

(*
 * $Log: exn-name.sml,v $
 * Revision 1.2  1998/11/18 03:54:13  jhr
 *  New array representations.
 *
 * Revision 1.1.1.1  1998/04/08 18:40:03  george
 * Version 110.5
 *
 *)
