(* export-sig.sml
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
 * $Log: export-sig.sml,v $
 * Revision 1.2  1997/04/10 14:34:53  dbm
 *   Changed return type of exportFn to unit.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:19  george
 *   Version 109.24
 *
 *)
