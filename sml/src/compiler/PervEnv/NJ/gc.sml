(* gc.sml
 *
 * COPYRIGHT (c) 1997 AT&T Labs Research.
 *
 * Garbage collector control and stats.
 *)

structure GC : GC =
  struct

    val gcCtl : ((string * int ref) list -> unit) =
	  CInterface.c_function "SMLNJ-RunT" "gcControl"

    fun doGC n = gcCtl [("DoGC", ref n)]

    fun messages true = gcCtl [("Messages", ref 1)]
      | messages false = gcCtl [("Messages", ref 0)]

  end


(*
 * $Log: gc.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:56  george
 * Version 110.5
 *
 *)
