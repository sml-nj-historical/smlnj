(* weak.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Weak :> WEAK =
  struct

  (** NOTE: this value must track the value given in the MachSpec.ObjDesc
   ** structure.  Eventually, we might make weak and strong into primops,
   ** so that we don't need to keep things synchronized.
   **)
    val special_weak = 2

    type 'a weak = 'a
    fun weak (x : 'a) : 'a weak = InlineT.mkspecial(special_weak, x)
    fun strong (x : 'a weak) : 'a option =
	  if InlineT.getspecial x = special_weak
	    then SOME(InlineT.PolyArray.sub(InlineT.cast x, 0))
	    else NONE

    type weak' = Assembly.object
    fun weak' x = InlineT.mkspecial(special_weak, x)
    fun strong' x = InlineT.getspecial x = special_weak
  end

(*
 * $Log: weak.sml,v $
 * Revision 1.2  1998/02/15 19:40:32  jhr
 *   Deleted SMLofNJ.Susp structure.
 *
 * Revision 1.1  1997/02/11 20:44:49  george
 *   Version 109.25.1
 *
 * Revision 1.2  1997/01/31  20:39:45  jhr
 * Replaced uses of "abstraction" with opaque signature matching.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:18  george
 *   Version 109.24
 *
 *)
