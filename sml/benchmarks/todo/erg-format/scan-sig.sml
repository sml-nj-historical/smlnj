(* scan-sig.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.
 *
 * Function to scan input. Similar syntax and semantics to C scanf.
 *
 * AUTHOR:  Emden Gansner
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    erg@ulysses.att.com
 *)

signature SCAN =
  sig
    structure F : FORMAT
    val scan : string -> string -> F.fitem_t list
    val scani : string -> (string * int) -> F.fitem_t list
  end

