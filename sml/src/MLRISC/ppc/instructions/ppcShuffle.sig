signature PPCSHUFFLE = sig
  structure I : PPCINSTR

  type t = {regmap:I.C.cell -> I.C.cell, tmp:I.ea option,
            dst:I.C.cell list, src:I.C.cell list}

  val shuffle : t -> I.instruction list
  val shufflefp : t -> I.instruction list
end
