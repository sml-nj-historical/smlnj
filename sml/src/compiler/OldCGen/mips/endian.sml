(* Copyright 1991 by AT&T Bell Laboratories 
 *
 *)
structure LittleEndian : ENDIAN = 
    struct
      val architecture = "mipsel"
      val bigEndian = false
      val >> = Word.>>
      val &  = Word.andb
      infix >> &

      val order_real = implode o rev o explode
      val low_order_offset = 0
      fun wordLayout (hi,lo) =
	(lo & 0w255, (lo >> 0w8) & 0w255, hi & 0w255, (hi >> 0w8) & 0w255)
    end

structure BigEndian : ENDIAN = 
    struct
      val architecture = "mipseb"
      val bigEndian = true
      val >> = Word.>>
      val &  = Word.andb
      infix >> &

      fun order_real x = x
      val low_order_offset = 1
      fun wordLayout (hi,lo) =
	((hi >> 0w8) & 0w255, hi & 0w255, (lo >> 0w8) & 0w255, lo & 0w255)
    end


(*
 * $Log: endian.sml,v $
 * Revision 1.1.1.1  1997/01/14 01:38:38  george
 *   Version 109.24
 *
 *)
