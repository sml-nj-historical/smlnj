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
	    then SOME(InlineT.recordSub(InlineT.cast x, 0))
	    else NONE

    type weak' = Assembly.object
    fun weak' x = InlineT.mkspecial(special_weak, x)
    fun strong' x = InlineT.getspecial x = special_weak
  end

(*
 * $Log: weak.sml,v $
 * Revision 1.2  1998/11/18 03:54:21  jhr
 *  New array representations.
 *
 * Revision 1.1.1.1  1998/04/08 18:39:56  george
 * Version 110.5
 *
 *)
