(* prot-db.sig
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
 * $Log: prot-db.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:57  george
 * Version 110.5
 *
 *)
