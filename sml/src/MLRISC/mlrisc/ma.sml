(* ma.sml --- graph coloring allocation of memory location
 * 
 * Copyright 1996 AT&T Bell Laboratories 
 *
 *)

signature MA_ARG = sig
  structure Liveness : LIVENESS
  structure InsnProps : INSN_PROPERTIES

  sharing Liveness.F.I = InsnProps.I


(*
 * $Log$
 *)
