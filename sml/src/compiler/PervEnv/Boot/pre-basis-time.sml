(* pre-basis-time.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This is the basis Time structure with only the time type, so that the
 * basis signatures can compile.  It has to be in a separate file from
 * pre-basis-structs.sml, since it depends on the binding of LargeInt.
 *)

structure Time =
  struct
    datatype time = TIME of {sec : LargeInt.int, usec : LargeInt.int}
  end;

(*
 * $Log: pre-basis-time.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:40:05  george
 * Version 110.5
 *
 *)
