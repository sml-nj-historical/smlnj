(* errormsg.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * $Log$
 * Revision 1.1.1.5  1999/04/17 16:22:23  monnier
 * version 110.11
 *
 * Revision 1.1.1.1  1998/04/08 18:40:05  george
 * Version 110.5
 *
 * Revision 1.1.1.1  1997/01/14 01:37:59  george
 *   Version 109.24
 *
 * Revision 1.1.1.2  1997/01/11  18:52:30  george
 *   ml-burg Version 109.24
 *
 * Revision 1.1.1.1  1996/01/31  16:01:24  george
 * Version 109
 * 
 *)

structure ErrorMsg = struct

  exception Compiler
  val anyErrors 	= ref false

  fun say (msg:string) 	= (print msg; print "\n")

  fun warning msg  	= say ("\tWarning: " ^ msg);

  fun complain msg 	= (say ("\tError: " ^ msg); anyErrors := true)

  fun impossible msg 	= (complain("Internal bug: " ^ msg); raise Compiler)
end
