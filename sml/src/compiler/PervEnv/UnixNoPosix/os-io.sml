(* os-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * NOTE: this interface has been proposed, but not yet adopted by the
 * Standard basis committee.
 *
 *)

structure OS_IO : OS_IO =
  struct
    type iodesc = PreBasis.iodesc
	(* an iodesc is an abstract descriptor for an OS object that
	 * supports I/O (e.g., file, tty device, socket, ...).
	 *)

    fun id (PreBasis.IODesc iod) = iod
	(* return an integer ID for the I/O descriptor; note that IDs may
	 * be reused during an execution, but that no two active I/O
	 * devices will have the same ID.
	 *)

(***** Don't have time to implement this stuff right now JHR [95-7-9] *****
    val kind : iodesc -> string
	(* return the kind of I/O descriptor; values include: "FILE",
	 * "PIPE", "SOCK", and "TTY".
	 *)

    type poll_desc
	(* this is an abstract representation of a polling operation on
	 * an I/O descriptor.
	 *)
    type poll_info
	(* this is an abstract representation of the per-descriptor
	 * information returned by the poll operation.
	 *)

    val pollDesc : iodesc -> poll_desc option
	(* create a polling operation on the given descriptor; note that
	 * not all I/O devices support polling.
	 *)
    val pollToIODesc : poll_desc -> iodesc
	(* return the I/O descriptor that is being polled *)

    exception Poll

  (* set polling events; if the polling operation is not appropriate
   * for the underlying I/O device, then the Poll exception is raised.
   *)
    val pollIn  : poll_desc -> poll_desc
    val pollOut : poll_desc -> poll_desc
    val pollPri : poll_desc -> poll_desc

  (* polling function *)
    val poll : (poll_desc list * Time.time option) -> poll_info list
	(* a timeout of NONE means wait indefinitely; a timeout of
	 * (SOME Time.zeroTime) means do not block.
	 *)

  (* check for conditions *)
    val isIn 		: poll_info -> bool
    val isOut		: poll_info -> bool
    val isPri		: poll_info -> bool
    val infoToPollDesc  : poll_info -> poll_desc

*****)

  end (* OS_IO *)


(*
 * $Log: os-io.sml,v $
 * Revision 1.2  1997/06/02 19:16:24  jhr
 *   SML'97 Basis Library changes (phase 2)
 *
 * Revision 1.1.1.1  1997/01/14  01:38:26  george
 *   Version 109.24
 *
 *)
