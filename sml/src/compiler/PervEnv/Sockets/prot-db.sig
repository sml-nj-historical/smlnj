(* prot-db-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature NET_PROT_DB =
  sig
    type entry
    val name : entry -> string
    val aliases : entry -> string list
    val protocol : entry -> int
    val getByName   : string -> entry option
    val getByNumber : int -> entry option
  end

(*
 * $Log: prot-db-sig.sml,v $
 * Revision 1.1.1.1  1997/01/14 01:38:24  george
 *   Version 109.24
 *
 *)
