(* gc.sig
 *
 * COPYRIGHT (c) 1997 AT&T Labs Research.
 *
 * Garbage collector control and stats.
 *)

signature GC =
  sig

    val doGC : int -> unit
    val messages : bool -> unit

  end


(*
 * $Log: gc.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:56  george
 * Version 110.5
 *
 *)
