(*
 * Functional units
 *)

signature FUNITS =
sig

   type fu

   val numberOfFUs : int
   val toString    : fu -> string
   val toInt       : fu -> int
   val fromInt     : int -> fu

end

(*
 * $Log: funits.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:48:27  george
 *   Version 110.10
 *
 *)
