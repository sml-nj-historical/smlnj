signature ALPHASHUFFLE = sig
  structure I : ALPHAINSTR

  type t = {tmp:I.ea option, dst:I.C.cell list, src:I.C.cell list}

  val shuffle : t -> I.instruction list
  val shufflefp : t -> I.instruction list
end

