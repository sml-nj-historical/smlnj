(* Copyright 1989 by AT&T Bell Laboratories *)
signature INTMAP =
  sig
    type 'a intmap
    val namednew : string * int * exn -> '1a intmap
    val new : int * exn -> '1a intmap
    val elems: 'a intmap -> int
    val add : '2a intmap -> int * '2a -> unit
    val rmv : 'a intmap -> int -> unit
    val map : 'a intmap -> int -> 'a
    val app : (int * 'a -> unit) -> 'a intmap -> unit
    val intMapToList: 'a intmap -> (int * 'a) list
    val clear : '1a intmap -> unit
  end

(*
 * $Log: intmap.sig,v $
 * Revision 1.1.1.1  1997/01/14  01:38:48  george
 *   Version 109.24
 *
 *)
