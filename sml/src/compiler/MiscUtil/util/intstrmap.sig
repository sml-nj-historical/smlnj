(* Copyright 1989 by AT&T Bell Laboratories *)
signature INTSTRMAP =
  sig
    type 'a intstrmap
    val namednew : string * int * exn -> '1a intstrmap
    val new : int * exn -> '1a intstrmap
    val elems : 'a intstrmap -> int
    val add : '2a intstrmap -> int * string * '2a -> unit
    val rmv : 'a intstrmap -> int * string -> unit
    val map : 'a intstrmap -> int * string -> 'a
    val app : (int * string * 'a -> unit) -> 'a intstrmap -> unit
    val intStrMapToList: 'a intstrmap -> (int * string * 'a) list
    val transform : ('a -> '2b) -> 'a intstrmap -> '2b intstrmap
  end

(*
 * $Log: intstrmap.sig,v $
 * Revision 1.1.1.1  1997/01/14  01:38:49  george
 *   Version 109.24
 *
 *)
