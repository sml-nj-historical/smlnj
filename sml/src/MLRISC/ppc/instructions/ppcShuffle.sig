signature PPCSHUFFLE = sig
  structure I : PPCINSTR

  type t = {regMap:int -> int, temp:I.ea option, dst:int list, src:int list}

  val shuffle : t -> I.instruction list
  val shufflefp : t -> I.instruction list
end
