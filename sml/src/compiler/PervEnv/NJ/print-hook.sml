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
 * Revision 1.1.1.1  1998/04/08 18:39:56  george
 * Version 110.5
 *
 *)

