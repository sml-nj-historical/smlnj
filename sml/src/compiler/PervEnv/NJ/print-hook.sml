(* print-hook.sml
 *
 * COPYRIGHT (c) 1997 AT&T Labs Research.
 *
 * This is a hook for the top-level print function, which allows
 * it to be rebound.
 *)

structure PrintHook =
  struct

    val prHook = ref TextIO.print

    fun print s = (! prHook) s

  end

(*
 * $Log: print-hook.sml,v $
 * Revision 1.1  1997/01/29 14:51:34  jhr
 * Added hook to allow dynamic rebinding of top-level print function.
 *
 *)

