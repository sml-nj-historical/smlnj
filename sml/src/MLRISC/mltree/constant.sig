(* constant.sml --- constants used to specialize MLRISC and the code generators.
 * 
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

signature CONSTANT = sig
  type const

  val toString : const -> string
  val valueOf : const -> int
end

(*
 * $Log: constant.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
