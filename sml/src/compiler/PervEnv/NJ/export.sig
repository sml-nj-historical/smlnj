(* export.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature EXPORT = 
  sig
    val exportML : string -> bool
    val exportFn : (string * ((string * string list) -> OS.Process.status)) -> unit
  end


(*
 * $Log: export.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:56  george
 * Version 110.5
 *
 *)
