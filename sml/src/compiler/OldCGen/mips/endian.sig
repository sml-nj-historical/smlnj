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
 * Revision 1.1.1.1  1997/01/14 01:38:38  george
 *   Version 109.24
 *
 *)
