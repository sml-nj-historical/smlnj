(* unsafe-mono-vector.sig
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

signature UNSAFE_MONO_VECTOR =
  sig

    type vector
    type elem

    val sub : (vector * int) -> elem
    val update : (vector * int * elem) -> unit
    val create : int -> vector

  end;


(*
 * $Log: unsafe-mono-vector.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:40:01  george
 * Version 110.5
 *
 *)
