(* unsafe-vector.sig
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

signature UNSAFE_VECTOR =
  sig

    val sub : ('a vector * int) -> 'a
    val create : (int * 'a list) -> 'a vector

  end;


(*
 * $Log: unsafe-vector.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:40:01  george
 * Version 110.5
 *
 *)
