signature CONST_TYPE = sig
  datatype root = Reg of int | Mem of int 

  datatype const = REGLIST of root list * int Intmap.intmap
  val toString : const -> string
  val valueOf : const -> int
end

(*
 * $Log$
 *)
