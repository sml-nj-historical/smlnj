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

