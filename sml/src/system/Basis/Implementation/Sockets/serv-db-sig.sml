(* serv-db-sig.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *
 * extracted from serv-db.mldoc (v. 1.0; 1998-06-05)
 *)

signature NET_SERV_DB =
  sig
    type entry
    val name : entry -> string
    val aliases : entry -> string list
    val port : entry -> int
    val protocol : entry -> string
    val getByName : string * string option -> entry option
    val getByPort : int * string option -> entry option
    
  end
