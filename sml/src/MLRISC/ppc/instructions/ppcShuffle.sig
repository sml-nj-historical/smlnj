signature PPCSHUFFLE = sig
  structure I : PPCINSTR

  type t = {regmap:I.C.register->I.C.register, tmp:I.ea option,                             dst:I.C.register list, src:I.C.register list}

  val shuffle : t -> I.instruction list
  val shufflefp : t -> I.instruction list
end
