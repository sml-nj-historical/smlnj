(* unix-sock-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * $Log$
 * Revision 1.2  2000/06/01 18:33:22  monnier
 * bring revisions from the vendor branch to the trunk
 *
 * Revision 1.1.1.1  1997/10/04 23:33:11  monnier
 *
 *
 * Revision 1.3  1996/09/06  00:35:43  george
 * *** empty log message ***
 *
 * Revision 1.2  1996/06/03  21:11:48  jhr
 * Sockets API cleanup.
 *
 * Revision 1.1.1.1  1996/01/31  16:02:40  george
 * Version 109
 * 
 *)

signature UNIX_SOCK =
  sig
    type unix

    type 'a sock = (unix, 'a) Socket.sock
    type 'a stream_sock = 'a Socket.stream sock
    type dgram_sock = Socket.dgram sock

    type sock_addr = unix Socket.sock_addr

    val unixAF : Socket.AF.addr_family   (* 4.3BSD internal protocols *)

    val toAddr   : string -> sock_addr
    val fromAddr : sock_addr -> string

    structure Strm : sig
	val socket     : unit -> 'a stream_sock
	val socketPair : unit -> ('a stream_sock * 'a stream_sock)
      end
    structure DGrm : sig
	val socket     : unit -> dgram_sock
	val socketPair : unit -> (dgram_sock * dgram_sock)
      end
  end;
