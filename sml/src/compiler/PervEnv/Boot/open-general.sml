(* open-general.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Open the General structure to give other basis modules access to its
 * contents, and also make the option type available at top-level.
 *
 *)

open General;

val op =  : ''a * ''a -> bool = InlineT.=;
val op <> : ''a * ''a -> bool = InlineT.<>;

datatype option = datatype Assembly.option;


(*
 * $Log$
 *)
