(* Copyright 1989 by AT&T Bell Laboratories *)
signature INTSTRMAP =
  sig
    type 'a intstrmap
    val namednew : string * int * exn -> '1a intstrmap
    val new : int * exn -> '1a intstrmap
    val elems : 'a intstrmap -> int
    val add : 'a intstrmap -> word * string * 'a -> unit
    val rmv : 'a intstrmap -> word * string -> unit
    val map : 'a intstrmap -> word * string -> 'a
    val app : (word * string * 'a -> unit) -> 'a intstrmap -> unit
    val intStrMapToList: 'a intstrmap -> (word * string * 'a) list
    val transform : ('a -> 'b) -> 'a intstrmap -> 'b intstrmap
  end

(*
 * $Log: intstrmap.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:14  george
 * Version 110.5
 *
 *)
