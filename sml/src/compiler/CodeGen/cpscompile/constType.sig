signature CONST_TYPE = sig
  datatype root = Reg of int | Mem of int 

  datatype const = REGLIST of root list * int Intmap.intmap
  val toString : const -> string
  val valueOf : const -> int
end

(*
 * $Log: constType.sig,v $
 * Revision 1.2  1997/07/17 12:36:48  george
 *   The constant type used to specialize MLTrees is now done more compactly.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:33  george
 *   Version 109.24
 *
 *)
