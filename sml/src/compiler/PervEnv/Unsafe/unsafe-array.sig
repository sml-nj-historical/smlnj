(* unsafe-array.sig
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

signature UNSAFE_ARRAY =
  sig

    val sub : ('a array * int) -> 'a
    val update : ('a array * int * 'a) -> unit
    val create : (int * 'a) -> 'a array

  end;


(*
 * $Log: unsafe-array.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:40:01  george
 * Version 110.5
 *
 *)
