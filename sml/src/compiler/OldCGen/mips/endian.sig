(* endian.sig
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature ENDIAN = 
    sig 
	val architecture : string
	val bigEndian : bool
	val order_real: string -> string
	val low_order_offset: int
	val wordLayout: word * word -> (word * word * word * word)
    end


(*
 * $Log: endian.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:47  george
 * Version 110.5
 *
 *)
