(* weak-sig.sml
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
 * $Log: weak-sig.sml,v $
 * Revision 1.2  1998/02/15 19:40:31  jhr
 *   Deleted SMLofNJ.Susp structure.
 *
 * Revision 1.1  1997/02/11 20:44:48  george
 *   Version 109.25.1
 *
 * Revision 1.1.1.1  1997/01/14  01:38:18  george
 *   Version 109.24
 *
 *)
