(*
 * Expandable set in bitvector format
 *)

signature BITSET =
sig

   type bitset 

   val create        : int -> bitset
   val size          : bitset -> int
   val contains      : bitset * int -> bool
   val set           : bitset * int -> unit
   val reset         : bitset * int -> unit
   val clear         : bitset -> unit
   val markAndTest   : bitset * int -> bool
   val unmarkAndTest : bitset * int -> bool
   val toString      : bitset -> string

end

(*
 * $Log: bitset.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:48:55  george
 *   Version 110.10
 *
 *)
