(* prot-db-sig.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *
 * extracted from prot-db.mldoc (v. 1.0; 1998-06-05)
 *)

signature NET_PROT_DB =
  sig
    type entry
    val name : entry -> string
    val aliases : entry -> string list
    val protocol : entry -> int
    val getByName : string -> entry option
    val getByNumber : int -> entry option
    
  end
