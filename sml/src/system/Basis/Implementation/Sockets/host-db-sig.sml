(* host-db-sig.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *
 * extracted from host-db.mldoc (v. 1.0; 1998-06-05)
 *)

signature NET_HOST_DB =
  sig
    eqtype in_addr
    eqtype addr_family
    type entry
    val name : entry -> string
    val aliases : entry -> string list
    val addrType : entry -> addr_family
    val addr : entry -> in_addr
    val addrs : entry -> in_addr list
    val getByName : string -> entry option
    val getByAddr : in_addr -> entry option
    val getHostName : unit -> string
    val scan : (char, 'a) StringCvt.reader -> (in_addr, 'a) StringCvt.reader
    val fromString : string -> in_addr option
    val toString : in_addr -> string
    
  end
