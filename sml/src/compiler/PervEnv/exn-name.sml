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

    val string_tag = 0x2a

  (* Normal exception names are strings; debugger exception names
   * are pairs of the form string * int.
   *)
    fun normalExnName (x : Assembly.object) = (InlineT.gettag x = string_tag)

    val exnName : exn -> string = InlineT.cast(
	  fn (ref s, _,_) => if normalExnName (InlineT.cast s)
		then s 
		else let val (s,_) = InlineT.cast s in s end)

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
 * Revision 1.4  1998/02/12 18:56:34  jhr
 *   Removed references to System.Tags.
 *
 * Revision 1.3  1997/06/30 19:36:19  jhr
 *   Removed System structure; added Unsafe structure.
 *
 * Revision 1.2  1997/03/19  20:49:13  george
 *   bugfix for 1105 -- message for uncaught exceptions
 *
 * Revision 1.1.1.1  1997/01/14  01:38:14  george
 *   Version 109.24
 *
 *)
