(* Copyright 1989 by AT&T Bell Laboratories *)
signature INTSTRMAP =
  sig
    type 'a intstrmap
    val namednew : string * int * exn -> '1a intstrmap
    val new : int * exn -> '1a intstrmap
    val elems : 'a intstrmap -> int
    val add : 'a intstrmap -> int * string * 'a -> unit
    val rmv : 'a intstrmap -> int * string -> unit
    val map : 'a intstrmap -> int * string -> 'a
    val app : (int * string * 'a -> unit) -> 'a intstrmap -> unit
    val intStrMapToList: 'a intstrmap -> (int * string * 'a) list
    val transform : ('a -> 'b) -> 'a intstrmap -> 'b intstrmap
  end

(*
 * $Log: intstrmap.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:14  george
 * Version 110.5
 *
 *)
