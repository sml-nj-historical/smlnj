(* unix-sock.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
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

(*
 * $Log: unix-sock.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:58  george
 * Version 110.5
 *
 *)
