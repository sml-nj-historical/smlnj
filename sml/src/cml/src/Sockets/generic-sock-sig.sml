(* generic-sock-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * $Log$
 * Revision 1.1  1997/10/04 23:33:11  monnier
 * Initial revision
 *
 * Revision 1.2  1996/06/03  21:11:42  jhr
 * Sockets API cleanup.
 *
 * Revision 1.1.1.1  1996/01/31  16:02:36  george
 * Version 109
 * 
 *)

signature GENERIC_SOCK =
  sig
    val addressFamilies : unit -> Socket.AF.addr_family list
	(* returns a list of the supported address families; this should include
	 * at least:  Socket.AF.inet.
	 *)

    val socketTypes : unit -> Socket.SOCK.sock_type
	(* returns a list of the supported socket types; this should include at
	 * least:  Socket.SOCK.stream and Socket.SOCK.dgram.
	 *)

  (* create sockets using default protocol *)
    val socket : (Socket.AF.addr_family * Socket.SOCK.sock_type)
	  -> ('a, 'b) Socket.sock
    val socketPair : (Socket.AF.addr_family * Socket.SOCK.sock_type)
	  -> (('a, 'b) Socket.sock * ('a, 'b) Socket.sock)

  (* create sockets using the specified protocol *)
    val socket' : (Socket.AF.addr_family * Socket.SOCK.sock_type * int)
	  -> ('a, 'b) Socket.sock
    val socketPair' : (Socket.AF.addr_family * Socket.SOCK.sock_type * int)
	  -> (('a, 'b) Socket.sock * ('a, 'b) Socket.sock)

  end
