(* endian.sig
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature ENDIAN = 
    sig 
	val order_real: string -> string
	val low_order_offset: int
	val wordLayout: int * int -> (int * int * int * int)
    end


(*
 * $Log: endian.sig,v $
 * Revision 1.1.1.1  1997/04/19 18:14:20  george
 *   Version 109.27
 *
 *)
