(* unsafe-mono-array.sig
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

signature UNSAFE_MONO_ARRAY =
  sig

    type array
    type elem

    val sub : (array * int) -> elem
    val update : (array * int * elem) -> unit
    val create : int -> array

  end;


(*
 * $Log: unsafe-mono-array.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:40:01  george
 * Version 110.5
 *
 *)
