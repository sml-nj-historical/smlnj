signature HPPA_MILLICODE = sig
  structure I : HPPAINSTR

  val divu : {rs:I.C.cell, rt:I.C.cell, rd:I.C.cell} -> I.instruction list
  val mulo : {rs:I.C.cell, rt:I.C.cell, rd:I.C.cell} -> I.instruction list
  val divo : {rs:I.C.cell, rt:I.C.cell, rd:I.C.cell} -> I.instruction list
  val mulu : {rs:I.C.cell, rt:I.C.cell, rd:I.C.cell} -> I.instruction list
  val cvti2s : {rs:I.C.cell, fd:I.C.cell} -> I.instruction list
  val cvti2d : {rs:I.C.cell, fd:I.C.cell} -> I.instruction list
  val cvti2q : {rs:I.C.cell, fd:I.C.cell} -> I.instruction list
end

