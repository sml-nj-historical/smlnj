(* serv-db.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature NET_SERV_DB =
  sig
    type entry
    val name : entry -> string
    val aliases : entry -> string list
    val port : entry -> int
    val protocol : entry -> string
    val getByName : (string  * string option) -> entry option
    val getByPort : (int  * string option) -> entry option
  end

(*
 * $Log: serv-db.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:57  george
 * Version 110.5
 *
 *)
