(* general.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure General : PRE_GENERAL =
  struct

    type unit = PrimTypes.unit
    type exn = PrimTypes.exn

    exception Bind = Core.Bind
    exception Match = Core.Match
    exception Subscript = Core.Subscript
    exception Size = Core.Size
    exception Overflow = Assembly.Overflow
    exception Chr = InlineT.Char.Chr
    exception Div = Assembly.Div
    exception Domain
    exception Span

    exception Fail of string

    datatype order = LESS | EQUAL | GREATER

    val ! = InlineT.!
    val op := = InlineT.:=

(*
    fun f o g = fn x => f(g x)
    fun a before b = a
*)
    val op o : ('b -> 'c) * ('a -> 'b) -> ('a -> 'c) = InlineT.compose
    val op before : ('a * unit) -> 'a = InlineT.before
    fun ignore _ = ()

  end (* structure General *)

(*
 * $Log: general.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:40:04  george
 * Version 110.5
 *
 *)
