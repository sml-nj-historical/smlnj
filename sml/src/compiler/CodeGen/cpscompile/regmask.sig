(* regmask.sig --- generation of masks for garbage collection.
 *
 * COPYRIGHT (c) 1997 Bell Laboratories.
 *
 *)

signature REGMASK = sig
  val regMask : int * word -> word
    (* given a register and mask returns the new mask *)

  val memMask : int * word -> word
    (* given a memory location and mask returns the new mask *)
end

(*
 * $Log: regmask.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:54  george
 * Version 110.5
 *
 *)
