(* weak.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)


signature WEAK = 
  sig

    type 'a weak
    val weak : 'a -> 'a weak
    val strong : 'a weak -> 'a option

    type weak'
    val weak' : 'a -> weak'
    val strong' : weak' -> bool

  end (* WEAK *)

(*
 * $Log: weak.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:56  george
 * Version 110.5
 *
 *)
