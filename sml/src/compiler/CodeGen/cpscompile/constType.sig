signature CONST_TYPE = sig
  datatype root = Reg of int | Mem of int 

  datatype const = REGLIST of root list * int Intmap.intmap
  val toString : const -> string
  val valueOf : const -> int
end

(*
 * $Log: constType.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:54  george
 * Version 110.5
 *
 *)
